-module(sockjs_filters).

-export([handle_req/3, dispatch/3]).
-export([xhr_polling/4, xhr_streaming/4, jsonp/4, iframe/4, eventsource/4,
         htmlfile/4, chunking_test/4, welcome_screen/4, options/4]).
-export([xhr_send/5, jsonp_send/5]).
-export([cache_for/4, h_sid/4, h_no_cache/4, xhr_cors/4, xhr_options/4,
         expect_xhr/4, expect_form/4]).
-export([chunking_loop/2]).

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
    Method = Req:get(method),
    case dispatch(Method, Path, Dispatcher) of
        {Receive, Server, SessionId, {SendRecv, Action, Filters}} ->
            io:format("~s ~s~n", [Method, Path]),
            Headers = lists:foldl(
                        fun (F, Headers0) ->
                                sockjs_filters:F(Req, Headers0,
                                                 Server, SessionId)
                        end, [], Filters),
            case SendRecv of
                send ->
                    sockjs_session:maybe_create(SessionId, Receive),
                    sockjs_filters:Action(Req, Headers, Server, SessionId);
                recv ->
                    try
                        sockjs_filters:Action(Req, Headers, Server, SessionId,
                                              Receive)
                    catch throw:no_session ->
                            Req:respond(404)
                    end
                end;
        nomatch ->
            nomatch;
        bad_method ->
            Req:respond(405)
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
     {p("/chunking_test"),           [{'POST',    send, chunking_test,  [h_sid, xhr_cors, expect_xhr]},
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
    headers(Req, Headers),
    reply_loop(Req, SessionId, true, fun fmt_xhr/1).

%% TODO Do something sensible with client closing timeouts
xhr_streaming(Req, Headers, _Server, SessionId) ->
    headers(Req, Headers),
    %% IE requires 2KB prefix:
    %% http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
    chunk(Req, list_to_binary(string:copies("h", 2048)), fun fmt_xhr/1),
    reply_loop(Req, SessionId, false, fun fmt_xhr/1).

jsonp(Req, Headers, _Server, SessionId) ->
    headers(Req, Headers),
    CB = callback(Req),
    reply_loop(Req, SessionId, true, fun (Body) -> fmt_jsonp(Body, CB) end).

iframe(Req, Headers, _Server, _SessionId) ->
    {ok, URL} = application:get_env(sockjs, sockjs_url),
    IFrame = fmt(?IFRAME, [URL]),
    MD5 = "\"" ++ binary_to_list(base64:encode(erlang:md5(IFrame))) ++ "\"",
    case header(Req, 'If-None-Match') of
        MD5 -> Req:respond(304, "");
        _   -> Req:ok([{"Content-Type", "text/html; charset=UTF-8"},
                       {"ETag",         MD5}] ++ Headers, IFrame)
    end.

eventsource(Req, Headers, _Server, SessionId) ->
    headers(Req, Headers, "text/event-stream; charset=UTF-8"),
    chunk(Req, <<$\r, $\n, $\r, $\n>>),
    reply_loop(Req, SessionId, true, fun fmt_eventsource/1).

htmlfile(Req, Headers, _Server, SessionId) ->
    headers(Req, Headers, "text/html; charset=UTF-8"),
    IFrame0 = fmt(?IFRAME_HTMLFILE, [callback(Req)]),
    %% Safari needs at least 1024 bytes to parse the website. Relevant:
    %%   http://code.google.com/p/browsersec/wiki/Part2#Survey_of_content_sniffing_behaviors
    Padding = list_to_binary(string:copies(" ", 1024 - size(IFrame0))),
    IFrame = <<IFrame0/binary, Padding/binary, $\r, $\n, $\r, $\n>>,
    chunk(Req, IFrame),
    reply_loop(Req, SessionId, false, fun fmt_htmlfile/1).

chunking_test(Req, Headers, _Server, _SessionId) ->
    headers(Req, Headers),
    Write = fun(P) -> chunk(Req, P, fun fmt_xhr/1) end,
    %% IE requires 2KB prelude
    Prelude = list_to_binary(string:copies(" ", 2048)),
    chunking_loop(Req, [{0,    Write, <<Prelude/binary, "h">>},
                        {5,    Write, <<"h">>},
                        {25,   Write, <<"h">>},
                        {125,  Write, <<"h">>},
                        {625,  Write, <<"h">>},
                        {3125, Write, <<"h">>}]).

chunking_loop(Req,  []) -> Req:chunk(done);
chunking_loop(Req, [{Timeout, Write, Payload} | Rest]) ->
    Write(Payload),
    timer:apply_after(Timeout, ?MODULE, chunking_loop, [Req, Rest]).

welcome_screen(Req, Headers, _Server, _SessionId) ->
    Req:ok([{"Content-Type", "text/plain; charset=UTF-8"}] ++ Headers,
           "Welcome to SockJS!\n").

options(Req, _Headers, _Server, _SessionId) ->
    Req:respond(204).

%% --------------------------------------------------------------------------

%% This is send but it receives - "send" from the client POV, receive
%% from ours.
xhr_send(Req, Headers, _Server, SessionId, Receive) ->
    receive_body(Req:get(body), SessionId, Receive),
    %% FF assumes that the response is XML.
    Req:respond(204, [{"content-type", "text/plain"}] ++ Headers, "").

jsonp_send(Req, Headers, _Server, SessionId, Receive) ->
    Body = proplists:get_value("d", Req:parse_post()),
    receive_body(Body, SessionId, Receive),
    Req:respond(200, Headers, "").

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
    case Req:get_cookie_value('JSESSIONID', Req:get_cookies()) of
        undefined -> [Req:set_cookie('JSESSIONID', "a") | Headers];
        _         -> Headers
    end.

h_no_cache(_Req, Headers, _Server, _SessionId) ->
    [{"Cache-Control", "no-store, no-cache, must-revalidate, max-age=0"}] ++
        Headers.

xhr_cors(Req, Headers, _Server, _SessionId) ->
    Origin = case header(Req, origin) of
                 undefined -> "*";
                 O         -> O
             end,
    AllowHeaders = case header(Req, 'access-control-request-headers') of
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

receive_body(Body, SessionId, Receive) ->
    Decoded = mochijson2:decode(Body),
    Sender = sockjs_session:sender(SessionId),
    [Receive(Sender, {recv, Msg}) || Msg <- Decoded].

header(Req, Name) ->
    proplists:get_value(Name, Req:get(headers)).

headers(Req, Headers) ->
    headers(Req, Headers, "application/javascript; charset=UTF-8").

headers(Req, Headers, ContentType) ->
    Req:chunk(head, [{"Content-Type", ContentType}] ++ Headers).

reply_loop(Req, SessionId, Once, Fmt) ->
    case sockjs_session:reply(SessionId) of
        wait           -> receive
                              go -> reply_loop(Req, SessionId, Once, Fmt)
                          after 5000 ->
                                  chunk(Req, <<"h">>, Fmt),
                                  reply_loop0(Req, SessionId, Once, Fmt)
                          end;
        session_in_use -> Err = sockjs_util:encode_list([{close, ?STILL_OPEN}]),
                          chunk(Req, Err, Fmt),
                          Req:chunk(done);
        Reply          -> chunk(Req, Reply, Fmt),
                          reply_loop0(Req, SessionId, Once, Fmt)
    end.

reply_loop0(Req, _SessionId, true, _Fmt) ->
    Req:chunk(done);
reply_loop0(Req, SessionId, false, Fmt) ->
    reply_loop(Req, SessionId, false, Fmt).

chunk(Req, Body)      -> Req:chunk(Body).
chunk(Req, Body, Fmt) -> chunk(Req, Fmt(Body)).

callback(Req) ->
    list_to_binary(proplists:get_value("c", Req:parse_qs())).

fmt_xhr(Body) -> <<Body/binary, $\n>>.

fmt_jsonp(Body, Callback) ->
    %% Yes, JSONed twice, there isn't a a better way, we must pass
    %% a string back, and the script, will be evaled() by the
    %% browser.
    Double = iolist_to_binary(mochijson2:encode(Body)),
    <<Callback/binary, "(", Double/binary, ");", $\r, $\n>>.

fmt_eventsource(Body) ->
    Escaped = iolist_to_binary(
                url_escape(binary_to_list(Body),
                           [$%, $\r, $\n, 0])), %% $% must be first!
    <<"data: ", Escaped/binary, $\r, $\n, $\r, $\n>>.

fmt_htmlfile(Body) ->
    Double = iolist_to_binary(mochijson2:encode(Body)),
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
