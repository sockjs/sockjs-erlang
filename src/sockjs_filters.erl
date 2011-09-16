-module(sockjs_filters).

-export([filters/1]).
-export([xhr_polling/5, xhr_streaming/5, xhr_send/5]).

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
     {xhr_streaming, [xhr_streaming]}].

%% --------------------------------------------------------------------------

xhr_send(Req, _Server, SessionId, xhr_send, Receive) ->
    Decoded = mochijson2:decode(Req:get(body)),
    Sender = sockjs_session:sender(SessionId),
    [Receive(Sender, {recv, Msg}) || Msg <- Decoded],
    Req:respond(204).

xhr_polling(Req, Server, SessionId, xhr_polling, Receive) ->
    case sockjs_session:reply(SessionId) of
        wait  -> receive
                     go -> xhr_polling(Req, Server, SessionId, xhr, Receive)
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
xhr_streaming(Req, Server, SessionId, xhr_streaming, Receive) ->
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
