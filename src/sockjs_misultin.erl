%% Adapter for misultin.
%% TODO: How to.

-module(sockjs_misultin).

-export([init_state/2]).

%% --------------------------------------------------------------------------

init_state(Fallback, DispatchTable) ->
    Loop =
        fun(Req0) ->
                Req = {misultin, Req0},
                {"/" ++ Path, Req1} = sockjs_http:path(Req),
                try
                    case sockjs_filters:handle_req(
                           Req1, Path, DispatchTable) of
                        nomatch -> Fallback(Req);
                        Req2    -> Req2
                    end
                catch A:B ->
                        Req:respond(500, [], "500")
                end
        end,
    WsLoop =
        fun(Ws) ->
                {"/" ++ Path, _Req1} = sockjs_http:path({misultin, Ws}),
                {Receive, _, _, _} = sockjs_filters:dispatch('GET', Path,
                                                             DispatchTable),
                sockjs_http:misultin_ws_loop(Ws, Receive)
        end,
    {Loop, WsLoop}.
