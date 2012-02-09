#!/usr/bin/env escript
%%! -smp disable +A1 +K true -pz ./ebin -pa deps/cowboy/ebin -input
-module(test_simple_cowboy).
-mode(compile).

-export([main/1]).

%% Cowboy callbacks
-export([init/3, handle/2, terminate/2]).


main(_) ->
    Port = 8081,
    application:start(sockjs),
    application:start(cowboy),

    SockjsState = sockjs_handler:init_state(
                    <<"/echo">>, fun service_echo/2, []),

    VhostRoutes = [{[<<"echo">>, '...'], sockjs_cowboy_handler, SockjsState},
                   {'_', ?MODULE, []}],
    Routes = [{'_',  VhostRoutes}], % any vhost

    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port,     Port}],
                          cowboy_http_protocol, [{dispatch, Routes}]),
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
