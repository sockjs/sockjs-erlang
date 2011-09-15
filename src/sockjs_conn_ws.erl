-module(sockjs_conn_ws).

-behaviour(sockjs_conn).

-export([send/2, close/3]).
-export([loop/2]).

send(Data, {sockjs_conn_ws, Ws}) ->
    Ws:send(["m", enc(Data)]).

close(Code, Reason, {sockjs_conn_ws, Ws}) ->
    Ws:send(["c", enc([Code, list_to_binary(Reason)])]),
    exit(normal). %% TODO ?

%% --------------------------------------------------------------------------

loop(Ws, Fun) ->
    Ws:send(["o"]),
    Self = {sockjs_conn_ws, Ws},
    Fun(Self, init),
    loop0(Ws, Fun, Self).

loop0(Ws, Fun, Self) ->
    receive
        {browser, Data} ->
            Decoded = mochijson2:decode(Data),
            Fun(Self, {recv, Decoded}),
            loop0(Ws, Fun, Self);
        closed ->
            closed;
        Msg ->
            Fun(Self, {info, Msg}),
            loop0(Ws, Fun, Self)
    end.

%% --------------------------------------------------------------------------

enc(Thing) ->
    iolist_to_binary(mochijson2:encode(Thing)).
