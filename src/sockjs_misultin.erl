%% Adapter for misultin.
%% TODO: How to.

-module(sockjs_misultin).

-export([init_state/2]).
-export([ws_loop/2]).

%% --------------------------------------------------------------------------

init_state(Fallback, DispatchTable) ->
    Loop =
        fun(Req0) ->
                Req = {misultin, Req0},
                {"/" ++ Path, Req1} = sockjs_http:path(Req),
                case sockjs_filters:handle_req(
                       Req1, Path, DispatchTable) of
                    nomatch -> Fallback(Req);
                    Req2    -> Req2
                end
        end,
    WsLoop =
        fun(Ws) ->
                {"/" ++ Path, _Req1} = sockjs_http:path({misultin, Ws}),
                {Receive, _, _, _} = sockjs_filters:dispatch('GET', Path,
                                                             DispatchTable),
                sockjs_misultin:ws_loop(Ws, Receive)
        end,
    {Loop, WsLoop}.

-define(WS_MODULE, sockjs_ws).

ws_loop(Ws, Receive) ->
    Self = {?WS_MODULE, Ws, misultin},
    ?WS_MODULE:open_frame(Self),
    Receive(Self, init),
    ws_loop0(Ws, Receive, Self).

ws_loop0(Ws, Receive, Self) ->
    receive
        {browser, ""} ->
            ws_loop0(Ws, Receive, Self);
        {browser, Data} ->
            case sockjs_util:decode(Data) of
                {ok, Decoded} ->
                    Receive(Self, {recv, Decoded}),
                    ws_loop0(Ws, Receive, Self);
                {error, _} ->
                    closed
            end;
        closed ->
            Receive(Self, closed),
            closed;
        Msg ->
            Receive(Self, {info, Msg}),
            ws_loop0(Ws, Receive, Self)
    end.
