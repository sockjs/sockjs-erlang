-module(sockjs_test).
-export([start/0, dispatcher/0]).

start() ->
    Port = 8080,
    application:start(sockjs),
    application:start(cowboy),
    Dispatch = [{'_', [{'_', sockjs_cowboy_handler, []}]}],
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port,     Port}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

dispatcher() ->
    [{echo,    fun test_echo/2},
     {close,   fun test_close/2},
     {amplify, fun test_amplify/2}].

test_echo(Conn, {recv, Data}) -> Conn:send(Data);
test_echo(_Conn, _)           -> ok.

test_close(Conn, _) ->
    Conn:close(3000, "Go away!").

test_amplify(Conn, {recv, Data}) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    Conn:send(list_to_binary(string:copies("x", round(math:pow(2, N)))));
test_amplify(_Conn, _) ->
    ok.
