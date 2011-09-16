-module(sockjs_filters).

-export([filters/1]).
-export([xhr_polling/4, xhr_streaming/4, xhr_send/4, jsonp/4, jsonp_send/4]).

filters(Endpoint) ->
    case proplists:get_value(list_to_atom(Endpoint), filters()) of
        undefined -> exit({unknown_endpoint, Endpoint});
        Filters   -> Filters
    end.

filters() ->
    %% websocket does not actually go via transport:handle_req/3 but we need
    %% something in transport:dispatch/2
    [{websocket,     []},
     {xhr_send,      [xhr_send]},
     {xhr,           [xhr_polling]},
     {xhr_streaming, [xhr_streaming]},
     {jsonp_send,    [jsonp_send]},
     {jsonp,         [jsonp]}].

%% --------------------------------------------------------------------------

xhr_send(Req, _Server, SessionId, Receive) ->
    receive_body(Req:get(body), SessionId, Receive),
    Req:respond(204).

receive_body(Body, SessionId, Receive) ->
    Decoded = mochijson2:decode(Body),
    Sender = sockjs_session:sender(SessionId),
    [Receive(Sender, {recv, Msg}) || Msg <- Decoded].

xhr_polling(Req, Server, SessionId, Receive) ->
    case sockjs_session:reply(SessionId) of
        wait  -> receive
                     go -> xhr_polling(Req, Server, SessionId, Receive)
                 after 5000 ->
                         reply(Req, <<"h">>)
                 end;
        Reply -> reply(Req, Reply)
    end.

reply(Req, Body) ->
    Req:ok([], <<Body/binary, $\n>>).

%% TODO commonality with xhr_polling?
%% TODO Detect client closing connection sanely - ATM we get an =ERROR
%% REPORT==== a few seconds after the client goes away
xhr_streaming(Req, Server, SessionId, Receive) ->
    Req:chunk(head,
              [{"Content-Type", "application/javascript; charset=UTF-8"}]),
    %% IE requires 2KB prefix:
    %% http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
    chunk(Req, list_to_binary(string:copies("h", 2048))),
    xhr_streaming0(Req, Server, SessionId, Receive).

xhr_streaming0(Req, Server, SessionId, Receive) ->
    case sockjs_session:reply(SessionId) of
        wait  -> receive
                     go -> ok
                 after 5000 ->
                         chunk(Req, <<"h">>)
                 end;
        Reply -> chunk(Req, Reply)
    end,
    xhr_streaming0(Req, Server, SessionId, Receive).

chunk(Req, Body) ->
    Req:chunk(<<Body/binary, $\n>>).

jsonp_send(Req, _Server, SessionId, Receive) ->
    receive_body(proplists:get_value("d", Req:parse_post()),
                 SessionId, Receive),
    Req:respond(200, "").

jsonp(Req, Server, SessionId, Receive) ->
    Callback = list_to_binary(proplists:get_value("c", Req:parse_qs())),
    case sockjs_session:reply(SessionId) of
        wait  -> receive
                     go -> jsonp(Req, Server, SessionId, Receive)
                 after 5000 ->
                         jsonp_reply(Req, Callback, <<"h">>)
                 end;
        Reply -> jsonp_reply(Req, Callback, Reply)
    end.

jsonp_reply(Req, Callback, Body) ->
    %% Yes, JSONed twice, there isn't a a better way, we must pass
    %% a string back, and the script, will be evaled() by the
    %% browser.
    Double = iolist_to_binary(mochijson2:encode(Body)),
    Req:ok([], <<Callback/binary, "(", Double/binary, ");", $\r, $\n>>).
