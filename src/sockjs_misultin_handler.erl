-module(sockjs_misultin_handler).

-export([handle_ws/2]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec handle_ws(service(), any()) -> normal.
handle_ws(Service = #service{logger = Logger}, Req) ->
    Req0 = Logger(Service, {misultin, Req}, websocket),

    {RawWebsocket, {misultin, Req2}} =
        case sockjs_handler:get_action(Service, Req0) of
            {{match, WS}, Req1} when WS =:= websocket orelse
                                     WS =:= rawwebsocket ->
                {WS, Req1}
        end,
    SessionPid = sockjs_session:maybe_create(undefined, Service),
    self() ! go,
    closed = handle_ws0({Req2, RawWebsocket, SessionPid}),
    sockjs_ws_handler:close(RawWebsocket, SessionPid),
    normal.

handle_ws0({_Req, RawWebsocket, SessionPid} = S) ->
    receive
        go ->
            case ws_loop(go, S) of
                ok       -> handle_ws0(S);
                shutdown -> closed
            end;
        {browser, Data} ->
            Data1 = list_to_binary(Data),
            case sockjs_ws_handler:received(RawWebsocket, SessionPid, Data1) of
                ok       -> handle_ws0(S);
                shutdown -> closed
            end;
        closed ->
            closed
    end.

ws_loop(go, {Req, RawWebsocket, SessionPid}) ->
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait          -> ok;
        {ok, Data}    -> self() ! go,
                         Req:send(Data),
                         ok;
        {close, <<>>} -> shutdown;
        {close, Data} -> Req:send(Data),
                         shutdown
    end.
