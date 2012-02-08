-module(sockjs_ws_handler).

-export([received/3, reply/2]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec received(websocket|rawwebsocket, pid(), binary()) -> ok | shutdown.
%% Ignore empty
received(_RawWebsocket, _SessionPid, <<>>) ->
    ok;
received(websocket, SessionPid, Data) ->
    case sockjs_json:decode(Data) of
        {ok, Msg} when is_binary(Msg) ->
            sockjs_session:received([Msg], SessionPid),
            ok;
        {ok, Messages} when is_list(Messages) ->
            sockjs_session:received(Messages, SessionPid),
            ok;
        _Else ->
            shutdown
    end;

received(rawwebsocket, SessionPid, Data) ->
    sockjs_session:received([Data], SessionPid).

-spec reply(websocket|rawwebsocket, pid()) -> {close|open, binary()} | wait.
reply(websocket, SessionPid) ->
    case sockjs_session:reply(SessionPid) of
        {W, Frame} when W =:= ok orelse W =:= close->
            Frame1 = sockjs_util:encode_frame(Frame),
            {W, iolist_to_binary(Frame1)};
        wait ->
            wait
    end;
reply(rawwebsocket, SessionPid) ->
    case sockjs_session:reply(SessionPid, false) of
        {W, Frame} when W =:= ok orelse W =:= close->
            case Frame of
                {open, nil}               -> reply(rawwebsocket, SessionPid);
                {close, {_Code, _Reason}} -> {close, <<>>};
                {data, [Msg]}             -> {ok, iolist_to_binary(Msg)}
            end;
        wait ->
            wait
    end.

