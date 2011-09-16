-module(sockjs_filters).

-export([filters/1]).
-export([xhr/5, xhr_send/5]).

filters(Endpoint) ->
    case proplists:get_value(list_to_atom(Endpoint), filters()) of
        undefined -> exit({unknown_endpoint, Endpoint});
        Filters   -> Filters
    end.

filters() ->
    %% websocket does not actually go via transport:handle_req/3 but we need
    %% something in transport:dispatch/2
    [{websocket, []},
     {xhr_send,  [xhr_send]},
     {xhr,       [xhr]}].

%% --------------------------------------------------------------------------

xhr_send(Req, _Server, SessionId, xhr_send, Receive) ->
    Decoded = mochijson2:decode(Req:get(body)),
    Sender = sockjs_session:sender(SessionId),
    [Receive(Sender, {recv, Msg}) || Msg <- Decoded],
    Req:respond(204).

xhr(Req, Server, SessionId, xhr, Receive) ->
    case sockjs_session:reply(SessionId) of
        wait  -> receive
                     go -> xhr(Req, Server, SessionId, xhr, Receive)
                 after 5000 ->
                         reply(Req, <<"h">>)
                 end;
        Reply -> reply(Req, Reply)
    end.

reply(Req, Body) ->
    Req:ok([], <<Body/binary, $\n>>).
