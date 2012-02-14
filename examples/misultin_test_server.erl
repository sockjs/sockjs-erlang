#!/usr/bin/env escript
%%! -smp disable +A1 +K true -pa ebin deps/misultin/ebin -input
-module(misultin_test_server).
-mode(compile).

-export([main/1]).

main(_) ->
    Port = 8081,
    application:start(sockjs),

    StateEcho = sockjs_handler:init_state(
                  <<"/echo">>, fun service_echo/3, state,
                  [{cookie_needed, true}, {response_limit, 4096}]),
    StateClose = sockjs_handler:init_state(
                   <<"/close">>, fun service_close/3, state, []),
    StateAmplify = sockjs_handler:init_state(
                     <<"/amplify">>, fun service_amplify/3, state, []),
    StateBroadcast = sockjs_handler:init_state(
                       <<"/broadcast">>, fun service_broadcast/3, state, []),
    StateDWSEcho = sockjs_handler:init_state(
                  <<"/disabled_websocket_echo">>, fun service_echo/3, state,
                     [{websocket, false}]),

    Services = [{"echo", StateEcho},
                {"close", StateClose},
                {"amplify", StateAmplify},
                {"broadcast", StateBroadcast},
                {"disabled_websocket_echo", StateDWSEcho}],

    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    misultin:start_link([{port, Port},
                         {autoexit, false},
                         {ws_autoexit, false},
                         {loop,    fun (Req) -> handle_http(Req, Services) end},
                         {ws_loop, fun (Req) -> handle_ws(Req, Services) end}]),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

handle_http(Req, Services) ->
    Prefix = case Req:resource([]) of
                 [H | _T] -> H;
                 []       -> nomatch
             end,
    case lists:keyfind(Prefix, 1, Services) of
        {Prefix, Service} ->
            sockjs_handler:handle_req(Service, {misultin, Req});
        false ->
            Req:respond(404,
                        <<"404 - Nothing here (via sockjs-erlang fallback)\n">>)
    end.

handle_ws(Req, Services) ->
    Prefix = case string:tokens(Req:get(path), "/") of
                 [H | _T] -> H;
                 []       -> nomatch
             end,
    case lists:keyfind(Prefix, 1, Services) of
        {Prefix, Service} ->
            sockjs_misultin_handler:handle_ws(Service, Req);
        false ->
            %% abort any other ws request
            closed
    end.

%% --------------------------------------------------------------------------

service_echo(Conn, {recv, Data}, _State) -> sockjs:send(Data, Conn);
service_echo(_Conn, _, _State)           -> ok.

service_close(Conn, _, _State) ->
    sockjs:close(3000, "Go away!", Conn).

service_amplify(Conn, {recv, Data}, _State) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    sockjs:send(list_to_binary(
                  string:copies("x", round(math:pow(2, N)))), Conn);
service_amplify(_Conn, _, _State) ->
    ok.

service_broadcast(Conn, init, _State) ->
    case ets:info(broadcast_table, memory) of
        undefined ->
            ets:new(broadcast_table, [public, named_table]);
        _Any ->
            ok
    end,
    true = ets:insert(broadcast_table, {Conn}),
    ok;
service_broadcast(Conn, closed, _State) ->
    true = ets:delete_object(broadcast_table, {Conn}),
    ok;
service_broadcast(_Conn, {recv, Data}, _State) ->
    ets:foldl(fun({Conn}, _Acc) -> sockjs:send(Data, Conn) end,
              [], broadcast_table),
    ok.
