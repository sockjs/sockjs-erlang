-module(sockjs_filters).

-export([handle_req/3, dispatch/3]).
-export([xhr_polling/4, xhr_streaming/4, jsonp/4, iframe/4, eventsource/4,
         htmlfile/4, chunking_test/4, welcome_screen/4, options/4]).
-export([xhr_send/5, jsonp_send/5]).
-export([cache_for/4, h_sid/4, h_no_cache/4, xhr_cors/4, xhr_options/4,
         expect_xhr/4, expect_form/4]).

-define(YEAR, 365 * 24 * 60 * 60).
-define(STILL_OPEN, {2010, "Another connection still open"}).

-define(IFRAME, "<!DOCTYPE html>
<html>
<head>
  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
  <script>
    document.domain = document.domain;
    _sockjs_onload = function(){SockJS.bootstrap_iframe();};
  </script>
  <script src=\"~s\"></script>
</head>
<body>
  <h2>Don't panic!</h2>
  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>
</body>
</html>").

-define(IFRAME_HTMLFILE, "<!doctype html>
<html><head>
  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
</head><body><h2>Don't panic!</h2>
  <script>
    document.domain = document.domain;
    var c = parent.~s;
    c.start();
    function p(d) {c.message(d);};
    window.onload = function() {c.stop();};
  </script>").

handle_req(Req, Path, Dispatcher) ->
    {Method, Req1} = sockjs_http:method(Req),
    case dispatch(Method, Path, Dispatcher) of
        {Receive, Server, SessionId, {SendRecv, Action, Filters}} ->
            %%io:format("~s ~s~n", [Method, Path]),
            Headers = lists:foldl(
                        fun (F, Headers0) ->
                                sockjs_filters:F(Req1, Headers0,
                                                 Server, SessionId)
                        end, [], Filters),
            case SendRecv of
                send ->
                    sockjs_session:maybe_create(SessionId, Receive),
                    sockjs_filters:Action(Req1, Headers, Server, SessionId);
                recv ->
                    try
                        sockjs_filters:Action(Req1, Headers, Server, SessionId,
                                              Receive)
                    catch throw:no_session ->
                            sockjs_http:reply(404, [], "", Req1)
                    end
                end;
        nomatch ->
            nomatch;
        bad_method ->
            sockjs_http:reply(405, [], "", Req1)
    end.

dispatch(Method, Path, Dispatcher) ->
    case lists:foldl(
           fun ({Match, MethodFilters}, nomatch) ->
                   case Match(Path) of
                       nomatch ->
                           nomatch;
                       Rest ->
                           case lists:keyfind(Method, 1, MethodFilters) of
                               false      -> bad_method;
                               {_, SR, A, Fs} -> [{SR, A, Fs} | Rest]
                           end
                   end;
               (_, A) ->
                   A
           end, nomatch, filters()) of
        [Filters, FunS, Server, Session] ->
            case proplists:get_value(list_to_atom(FunS), Dispatcher) of
                undefined -> nomatch;
                Fun       -> {Fun, Server, Session, Filters}
            end;
        Else ->
            Else
    end.

filters() ->
    OptsFilters = [h_sid, xhr_cors, cache_for, xhr_options],
    %% websocket does not actually go via handle_req/3 but we need
    %% something in dispatch/2
    [{t("/websocket"),               [{'GET',     send, dummy,          []}]},
     {t("/xhr_send"),                [{'POST',    recv, xhr_send,       [h_sid, xhr_cors, expect_xhr]},
                                      {'OPTIONS', send, options,        OptsFilters}]},
     {t("/xhr"),                     [{'POST',    send, xhr_polling,    [h_sid, xhr_cors]},
                                      {'OPTIONS', send, options,        OptsFilters}]},
     {t("/xhr_streaming"),           [{'POST',    send, xhr_streaming,  [h_sid, xhr_cors]},
                                      {'OPTIONS', send, options,        OptsFilters}]},
     {t("/jsonp_send"),              [{'POST',    recv, jsonp_send,     [h_sid, expect_form]}]},
     {t("/jsonp"),                   [{'GET',     send, jsonp,          [h_sid, h_no_cache]}]},
     {t("/eventsource"),             [{'GET',     send, eventsource,    [h_sid, h_no_cache]}]},
     {t("/htmlfile"),                [{'GET',     send, htmlfile,       [h_sid, h_no_cache]}]},
     {p(""),                         [{'GET',     send, welcome_screen, []}]},
     {p("/iframe[0-9-.a-z_]*.html"), [{'GET',     send, iframe,         [cache_for]}]},
     {p("/chunking_test"),           [{'POST',    send, chunking_test,  [xhr_cors, expect_xhr]},
                                      {'OPTIONS', send, options,        OptsFilters}]}
    ].

%% TODO make relocatable (here?)
p(S) -> fun (Path) -> re(Path, "^([^/.]+)" ++ S ++ "[/]?\$") end.
t(S) -> fun (Path) -> re(Path, "^([^/.]+)/([^/.]+)/([^/.]+)" ++ S ++ "\$") end.

re(Path, S) ->
    case re:run(Path, S, [{capture, all_but_first, list}]) of
        nomatch                          -> nomatch;
        {match, [FunS]}                  -> [FunS, dummy, dummy];
        {match, [FunS, Server, Session]} -> [FunS, Server, Session]
    end.

%% --------------------------------------------------------------------------

xhr_polling(Req, Headers, _Server, SessionId) ->
    Req1 = headers(Req, Headers),
    reply_loop(Req1, SessionId, true, fun fmt_xhr/1).

%% TODO Do something sensible with client closing timeouts
xhr_streaming(Req, Headers, _Server, SessionId) ->
    Req1 = headers(Req, Headers),
    %% IE requires 2KB prefix:
    %% http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
    chunk(Req1, list_to_binary(string:copies("h", 2048)), fun fmt_xhr/1),
    reply_loop(Req1, SessionId, false, fun fmt_xhr/1).

jsonp(Req, Headers, _Server, SessionId) ->
    S = fun (CB) ->
                Req1 = headers(Req, Headers),
                reply_loop(Req1, SessionId, true,
                           fun (Body) -> fmt_jsonp(Body, CB) end)
        end,
    verify_callback(Req, S).

iframe(Req, Headers, _Server, _SessionId) ->
    {ok, URL} = application:get_env(sockjs, sockjs_url),
    IFrame = fmt(?IFRAME, [URL]),
    MD5 = "\"" ++ binary_to_list(base64:encode(erlang:md5(IFrame))) ++ "\"",
    case sockjs_http:header('If-None-Match', Req) of
        MD5 -> sockjs_http:reply(304, [], "", Req);
        _   -> sockjs_http:reply(
                 200, [{"Content-Type", "text/html; charset=UTF-8"},
                       {"ETag",         MD5}] ++ Headers, IFrame, Req)
    end.

eventsource(Req, Headers, _Server, SessionId) ->
    Req1 = headers(Req, Headers, "text/event-stream; charset=UTF-8"),
    chunk(Req1, <<$\r, $\n>>),
    reply_loop(Req1, SessionId, false, fun fmt_eventsource/1).

htmlfile(Req, Headers, _Server, SessionId) ->
    S = fun (CB) ->
                Req1 = headers(Req, Headers, "text/html; charset=UTF-8"),
                IFrame0 = fmt(?IFRAME_HTMLFILE, [CB]),
                %% Safari needs at least 1024 bytes to parse the
                %% website. Relevant:
                %%   http://code.google.com/p/browsersec/wiki/Part2#Survey_of_content_sniffing_behaviors
                Padding = list_to_binary(string:copies(" ", 1024 - size(IFrame0))),
                IFrame = <<IFrame0/binary, Padding/binary, $\r, $\n, $\r, $\n>>,
                chunk(Req1, IFrame),
                reply_loop(Req1, SessionId, false, fun fmt_htmlfile/1)
        end,
    verify_callback(Req, S).

verify_callback(Req, Success) ->
    {CB, Req1} = sockjs_http:callback(Req),
    case CB of
        undefined ->
            sockjs_http:reply(500, [], "\"callback\" parameter required", Req);
        _ ->
            Success(CB)
    end.

chunking_test(Req, Headers, _Server, _SessionId) ->
    Req1 = headers(Req, Headers),
    %% IE requires 2KB prelude
    Prelude = list_to_binary(string:copies(" ", 2048)),
    chunking_loop(Req1, [{0,    <<Prelude/binary, "h">>},
                         {5,    <<"h">>},
                         {25,   <<"h">>},
                         {125,  <<"h">>},
                         {625,  <<"h">>},
                         {3125, <<"h">>}]).

chunking_loop(Req,  []) ->
    sockjs_http:chunk_end(Req),
    Req;
chunking_loop(Req, [{Timeout, Payload} | Rest]) ->
    timer:sleep(Timeout),
    R = chunk(Req, Payload, fun fmt_xhr/1),
    Rest1 = case R of
                ok -> Rest;
                _ -> []
            end,
    chunking_loop(Req, Rest1).

welcome_screen(Req, Headers, _Server, _SessionId) ->
    sockjs_http:reply(200, [{"Content-Type", "text/plain; charset=UTF-8"}] ++ Headers,
          "Welcome to SockJS!\n", Req).

options(Req, Headers, _Server, _SessionId) ->
    sockjs_http:reply(204, Headers, "", Req).

%% --------------------------------------------------------------------------
%% This is send but it receives - "send" from the client POV, receive
%% from ours.
xhr_send(Req, Headers, _Server, SessionId, Receive) ->
    {Body, Req1} = sockjs_http:body(Req),
    Success = fun () -> sockjs_http:reply(204,
                                          [{"content-type", "text/plain"}] ++
                                              Headers, "", Req1) end,
    verify_body(Req1, Body, SessionId, Receive, Success).

jsonp_send(Req, Headers, _Server, SessionId, Receive) ->
    {Body, Req1} = sockjs_http:body_qs(Req),
    Success = fun () -> sockjs_http:reply(200, Headers, "ok", Req) end,
    verify_body(Req1, Body, SessionId, Receive, Success).

verify_body(Req, Body, SessionId, Receive, Success) ->
    case Body of
        E when E =:= undefined orelse E =:= [] orelse E =:= <<>> ->
            sockjs_http:reply(500, [],
                              "Payload expected.", Req);
        _ ->
            case sockjs_util:decode(Body) of
                 {ok, Decoded} ->
                     receive_body(Decoded, SessionId, Receive),
                     Success();
                 {error, _} ->
                     sockjs_http:reply(500, [],
                                       "Broken JSON encoding.", Req)
             end
    end.
%% --------------------------------------------------------------------------

cache_for(_Req, Headers, _Server, _SessionId) ->
    Expires = calendar:gregorian_seconds_to_datetime(
                calendar:datetime_to_gregorian_seconds(
                  calendar:now_to_datetime(now())) + ?YEAR),
    [{"Cache-Control", "public, max-age=" ++ integer_to_list(?YEAR)},
     {"Expires",       httpd_util:rfc1123_date(Expires)}] ++ Headers.

h_sid(Req, Headers, _Server, _SessionId) ->
    %% Some load balancers do sticky sessions, but only if there is
    %% a JSESSIONID cookie. If this cookie isn't yet set, we shall
    %% set it to a dumb value. It doesn't really matter what, as
    %% session information is usually added by the load balancer.
    case sockjs_http:jsessionid(Req) of
        undefined -> [{"Set-Cookie", "JSESSIONID=dummy; path=/"}];
        _         -> []
    end ++ Headers.

h_no_cache(_Req, Headers, _Server, _SessionId) ->
    [{"Cache-Control", "no-store, no-cache, must-revalidate, max-age=0"}] ++
        Headers.

xhr_cors(Req, Headers, _Server, _SessionId) ->
    Origin = case sockjs_http:header('Origin', Req) of
                 undefined -> "*";
                 O         -> O
             end,
    AllowHeaders = case sockjs_http:header(
                          'Access-Control-Request-Headers', Req) of
                       undefined -> [];
                       V         -> [{"Access-Control-Allow-Headers", V}]
                   end,
    [{"Access-Control-Allow-Origin",      Origin},
     {"Access-Control-Allow-Credentials", "true"}] ++ AllowHeaders ++ Headers.

xhr_options(_Req, Headers, _Server, _SessionId) ->
    [{"Allow",                  "OPTIONS, POST"},
     {"Access-Control-Max-Age", integer_to_list(?YEAR)}] ++ Headers.

expect_xhr(_Req, Headers, _Server, _SessionId) ->
    Headers. %% TODO

expect_form(_Req, Headers, _Server, _SessionId) ->
    Headers. %% TODO

%% --------------------------------------------------------------------------

receive_body(Decoded, SessionId, Receive) ->
    Sender = sockjs_session:sender(SessionId),
    [Receive(Sender, {recv, Msg}) || Msg <- Decoded].

headers(Req, Headers) ->
    headers(Req, Headers, "application/javascript; charset=UTF-8").

headers(Req, Headers, ContentType) ->
    sockjs_http:chunk_start(200, [{"Content-Type", ContentType}] ++ Headers,
                            Req).

reply_loop(Req, SessionId, Once, Fmt) ->
    {ok, Heartbeat} = application:get_env(sockjs, heartbeat_ms),
    case sockjs_session:reply(SessionId, Once) of
        wait           -> receive
                              go -> reply_loop(Req, SessionId, Once, Fmt)
                          after Heartbeat ->
                                  chunk(Req, <<"h">>, Fmt),
                                  reply_loop0(Req, SessionId, Once, Fmt)
                          end;
        session_in_use -> Err = sockjs_util:encode_list([{close, ?STILL_OPEN}]),
                          chunk(Req, Err, Fmt),
                          sockjs_http:chunk_end(Req);
        Reply          -> chunk(Req, Reply, Fmt),
                          reply_loop0(Req, SessionId, Once, Fmt)
    end.

reply_loop0(Req, _SessionId, true, _Fmt) ->
    sockjs_http:chunk_end(Req);
reply_loop0(Req, SessionId, false, Fmt) ->
    reply_loop(Req, SessionId, false, Fmt).

chunk(Req, Body)      -> sockjs_http:chunk(Body, Req).
chunk(Req, Body, Fmt) -> chunk(Req, Fmt(Body)).

fmt_xhr(Body) -> <<Body/binary, $\n>>.

fmt_jsonp(Body, Callback) ->
    %% Yes, JSONed twice, there isn't a a better way, we must pass
    %% a string back, and the script, will be evaled() by the
    %% browser.
    Double = sockjs_util:encode(Body),
    <<Callback/binary, "(", Double/binary, ");", $\r, $\n>>.

fmt_eventsource(Body) ->
    Escaped = iolist_to_binary(
                url_escape(binary_to_list(Body),
                           [$%, $\r, $\n, 0])), %% $% must be first!
    <<"data: ", Escaped/binary, $\r, $\n, $\r, $\n>>.

fmt_htmlfile(Body) ->
    Double = sockjs_util:encode(Body),
    <<"<script>", $\n, "p(", Double/binary, ");", $\n, "</script>", $\r, $\n>>.

fmt(Fmt, Args) -> iolist_to_binary(io_lib:format(Fmt, Args)).

url_escape("", _Chars) ->
    "";
url_escape([Char | Rest], Chars) ->
    case lists:member(Char, Chars) of
        true  -> [hex(Char) | url_escape(Rest, Chars)];
        false -> [Char | url_escape(Rest, Chars)]
    end.

hex(C) ->
    <<High0:4, Low0:4>> = <<C>>,
    High = integer_to_list(High0),
    Low = integer_to_list(Low0),
    "%" ++ High ++ Low.
