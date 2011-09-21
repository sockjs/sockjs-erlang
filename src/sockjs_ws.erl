-module(sockjs_ws).

-behaviour(sockjs_sender).

-export([send/2, close/3]).
-export([loop/2]).

%% TODO this has little in common with the other transports
%% Where should framing happen? (Do we care?)

send(Data, {?MODULE, Ws}) ->
    Ws:send(["m", sockjs_util:enc(Data)]).

close(Code, Reason, {?MODULE, Ws}) ->
    Ws:send(["c", sockjs_util:enc([Code, list_to_binary(Reason)])]),
    exit(normal). %% TODO ?

%% --------------------------------------------------------------------------

loop(Ws, Receive) ->
    Ws:send(["o"]),
    Self = {?MODULE, Ws},
    Receive(Self, init),
    loop0(Ws, Receive, Self).

loop0(Ws, Receive, Self) ->
    receive
        {browser, Data} ->
            Decoded = mochijson2:decode(Data),
            Receive(Self, {recv, Decoded}),
            loop0(Ws, Receive, Self);
        closed ->
            Receive(Self, closed),
            closed;
        Msg ->
            Receive(Self, {info, Msg}),
            loop0(Ws, Receive, Self)
    end.
