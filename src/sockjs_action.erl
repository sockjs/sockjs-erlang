-module(sockjs_action).

-export([welcome_screen/3, options/3, iframe/3, info_test/3]).

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

-spec welcome_screen(req(), headers(), state()) -> req().
welcome_screen(Req, Headers, _State) ->
    H = [{"Content-Type", "text/plain; charset=UTF-8"}],
    sockjs_http:reply(200, H ++ Headers,
          "Welcome to SockJS!\n", Req).

-spec options(req(), headers(), state()) -> req().
options(Req, Headers, _State) ->
    sockjs_http:reply(204, Headers, "", Req).

-spec iframe(req(), headers(), state()) -> req().
iframe(Req, Headers, #state{url = Url}) ->
    IFrame = io_lib:format(?IFRAME, [Url]),
    MD5 = "\"" ++ binary_to_list(base64:encode(erlang:md5(IFrame))) ++ "\"",
    {H, Req2} = sockjs_http:header('If-None-Match', Req),
    case H of
        MD5 -> sockjs_http:reply(304, Headers, "", Req2);
        _   -> sockjs_http:reply(
                 200, [{"Content-Type", "text/html; charset=UTF-8"},
                       {"ETag",         MD5}] ++ Headers, IFrame, Req2)
    end.


-spec info_test(req(), headers(), state()) -> req().
info_test(Req, Headers, #state{websocket = Websocket,
                               cookie_needed = CookieNeeded}) ->
    I = [{websocket, Websocket},
         {cookie_needed, CookieNeeded},
         {origins, ['*:*']},
         {entropy, random:uniform(erlang:trunc(math:pow(2,32)))-1}],
    D = sockjs_json:encode(I),
    H = [{"Content-Type", "application/json; charset=UTF-8"}],
    sockjs_http:reply(200, H ++ Headers, D, Req).

