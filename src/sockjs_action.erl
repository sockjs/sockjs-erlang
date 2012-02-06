-module(sockjs_action).

% none
-export([welcome_screen/3, options/3, iframe/3, info_test/3]).
% send
-export([xhr_polling/4, xhr_streaming/4]).
% recv
-export([xhr_send/4]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

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

%% --------------------------------------------------------------------------

-spec welcome_screen(req(), headers(), service()) -> req().
welcome_screen(Req, Headers, _Service) ->
    H = [{"Content-Type", "text/plain; charset=UTF-8"}],
    sockjs_http:reply(200, H ++ Headers,
          "Welcome to SockJS!\n", Req).

-spec options(req(), headers(), service()) -> req().
options(Req, Headers, _Service) ->
    sockjs_http:reply(204, Headers, "", Req).

-spec iframe(req(), headers(), service()) -> req().
iframe(Req, Headers, #service{url = Url}) ->
    IFrame = io_lib:format(?IFRAME, [Url]),
    MD5 = "\"" ++ binary_to_list(base64:encode(erlang:md5(IFrame))) ++ "\"",
    {H, Req2} = sockjs_http:header('If-None-Match', Req),
    case H of
        MD5 -> sockjs_http:reply(304, Headers, "", Req2);
        _   -> sockjs_http:reply(
                 200, [{"Content-Type", "text/html; charset=UTF-8"},
                       {"ETag",         MD5}] ++ Headers, IFrame, Req2)
    end.


-spec info_test(req(), headers(), service()) -> req().
info_test(Req, Headers, #service{websocket = Websocket,
                                 cookie_needed = CookieNeeded}) ->
    I = [{websocket, Websocket},
         {cookie_needed, CookieNeeded},
         {origins, ['*:*']},
         {entropy, sockjs_util:rand32()}],
    D = sockjs_json:encode(I),
    H = [{"Content-Type", "application/json; charset=UTF-8"}],
    sockjs_http:reply(200, H ++ Headers, D, Req).

%% --------------------------------------------------------------------------

-spec xhr_polling(req(), headers(), service(), session()) -> req().
xhr_polling(Req, Headers, Service, Session) ->
    Req1 = chunk_start(Req, Headers),
    reply_loop(Req1, Session, 1, fun fmt_xhr/1, Service).

-spec xhr_streaming(req(), headers(), service(), session()) -> req().
xhr_streaming(Req, Headers, Service = #service{response_limit = ResponseLimit},
              Session) ->
    Req1 = chunk_start(Req, Headers),
    %% IE requires 2KB prefix:
    %% http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
    {_, Req2} = chunk(Req1, list_to_binary(string:copies("h", 2048)),
                      fun fmt_xhr/1),
    reply_loop(Req2, Session, ResponseLimit, fun fmt_xhr/1, Service).

%% --------------------------------------------------------------------------

-spec xhr_send(req(), headers(), service(), session()) -> req().
xhr_send(Req, Headers, _Service, Session) ->
    {Body, Req1} = sockjs_http:body(Req),
    case handle_recv(Req1, Body, Session) of
        {error, Req2} ->
            Req2;
        ok ->
            H = [{"content-type", "text/plain; charset=UTF-8"}],
            sockjs_http:reply(204, H ++ Headers, "", Req1)
    end.

handle_recv(Req, Body, Session) ->
    case Body of
        _Any when Body =:= <<>> ->
            {error, sockjs_http:reply(500, [], "Payload expected.", Req)};
        _Any ->
            case sockjs_json:decode(Body) of
                {ok, Decoded} ->
                    sockjs_session:received(Decoded, Session),
                    ok;
                {error, _} ->
                    {error, sockjs_http:reply(500, [],
                                              "Broken JSON encoding.", Req)}
            end
    end.

%% --------------------------------------------------------------------------

-define(STILL_OPEN, {2010, "Another connection still open"}).

chunk_start(Req, Headers) ->
    chunk_start(Req, Headers, "application/javascript; charset=UTF-8").
chunk_start(Req, Headers, ContentType) ->
    sockjs_http:chunk_start(200, [{"Content-Type", ContentType}] ++ Headers,
                            Req).

reply_loop(Req, SessionId, ResponseLimit, Fmt,
           Service = #service{heartbeat_delay = Heartbeat}) ->
    case sockjs_session:reply(SessionId) of
        wait           -> receive
                              go -> reply_loop(Req, SessionId, ResponseLimit,
                                               Fmt, Service)
                          after Heartbeat ->
                                  {_, Req2} = chunk(Req, <<"h">>, Fmt),
                                  reply_loop0(Req2, SessionId, ResponseLimit,
                                              Fmt, Service)
                          end;
        session_in_use -> Err = sockjs_util:encode_frame({close, ?STILL_OPEN}),
                          {ok, Req2} = chunk(Req, Err, Fmt),
                          sockjs_http:chunk_end(Req2);
        Reply          ->
            Reply2 = iolist_to_binary(Reply),
            {_, Req2} = chunk(Req, Reply2, Fmt),
            reply_loop0(Req2, SessionId,
                        ResponseLimit - byte_size(Reply2),
                        Fmt, Service)
    end.

reply_loop0(Req, _SessionId, ResponseLimit, _Fmt, _Service) when ResponseLimit =< 0 ->
    sockjs_http:chunk_end(Req);
reply_loop0(Req, SessionId, ResponseLimit, Fmt, Service) ->
    reply_loop(Req, SessionId, ResponseLimit, Fmt, Service).

chunk(Req, Body)      -> sockjs_http:chunk(Body, Req).
chunk(Req, Body, Fmt) -> chunk(Req, Fmt(Body)).

-spec fmt_xhr(iodata()) -> iodata().
fmt_xhr(Body) -> [Body, "\n"].
