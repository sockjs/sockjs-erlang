#!/usr/bin/env escript
%%! -smp disable +A1 +K true -pz ./ebin -pa deps/cowboy/ebin -input
-mode(compile).

-export([main/1]).

%% Cowboy callbacks
-export([init/3, handle/2, terminate/2]).


main(_) ->
    Port = 8081,
    application:start(sockjs),
    application:start(cowboy),

    StateEcho = sockjs_handler:init_state(
                  <<"/echo">>, fun service_echo/2, [{cookie_needed, true},
                                                    {response_limit, 4096}]),
    StateClose = sockjs_handler:init_state(
                   <<"/close">>, fun service_close/2, []),
    StateAmplify = sockjs_handler:init_state(
                     <<"/amplify">>, fun service_amplify/2, []),
    StateBroadcast = sockjs_handler:init_state(
                       <<"/broadcast">>, fun service_broadcast/2, []),
    StateDWSEcho = sockjs_handler:init_state(
                  <<"/disabled_websocket_echo">>, fun service_echo/2,
                     [{websocket, false}]),

    VRoutes = [{[<<"echo">>, '...'], sockjs_cowboy_handler, StateEcho},
               {[<<"close">>, '...'], sockjs_cowboy_handler, StateClose},
               {[<<"amplify">>, '...'], sockjs_cowboy_handler, StateAmplify},
               {[<<"broadcast">>, '...'], sockjs_cowboy_handler, StateBroadcast},
               {[<<"disabled_websocket_echo">>, '...'], sockjs_cowboy_handler,
                StateDWSEcho},
               {'_', ?MODULE, []}],
    Routes = [{'_',  VRoutes}], % any vhost
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port,     Port}],
                          cowboy_http_protocol, [{dispatch, Routes}]),
    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [],
                 <<"404 - Nothing here (via sockjs-erlang fallback)\n">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

service_echo(Conn, {recv, Data}) -> sockjs:send(Data, Conn);
service_echo(_Conn, _)           -> ok.

service_close(Conn, _) ->
    sockjs:close(3000, "Go away!", Conn).

service_amplify(Conn, {recv, Data}) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    sockjs:send(list_to_binary(
                  string:copies("x", round(math:pow(2, N)))), Conn);
service_amplify(_Conn, _) ->
    ok.

service_broadcast(Conn, init) ->
    case ets:info(broadcast_table, memory) of
        undefined ->
            ets:new(broadcast_table, [public, named_table]);
        _Any ->
            ok
    end,
    true = ets:insert(broadcast_table, {Conn}),
    ok;
service_broadcast(Conn, closed) ->
    true = ets:delete_object(broadcast_table, {Conn}),
    ok;
service_broadcast(_Conn, {recv, Data}) ->
    ets:foldl(fun({Conn}, _Acc) -> sockjs:send(Data, Conn) end,
              [], broadcast_table),
    ok.
