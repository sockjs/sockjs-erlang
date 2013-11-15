#!/usr/bin/env escript
%%! -smp disable +A1 +K true -pa ebin -env ERL_LIBS deps -input
-module(cowboy_echo).
-mode(compile).

-export([main/1]).

%% Cowboy callbacks
-export([init/3, handle/2, terminate/3]).


main(_) ->
    Port = 8081,
    ok = application:start(xmerl),
    ok = application:start(sockjs),
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowboy),

    SockjsState = sockjs_handler:init_state(
                    <<"/echo">>, fun service_echo/3, state, []),

    VhostRoutes = [{<<"/echo/[...]">>, sockjs_cowboy_handler, SockjsState},
                   {'_', ?MODULE, []}],
    Routes = [{'_',  VhostRoutes}], % any vhost
    Dispatch = cowboy_router:compile(Routes),

    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    cowboy:start_http(cowboy_echo_http_listener, 100, 
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

init({_Any, http}, Req, []) ->
    {ok, Req, []}.

handle(Req, State) ->
    {ok, Data} = file:read_file("./examples/echo.html"),
    {ok, Req1} = cowboy_req:reply(200, [{<<"Content-Type">>, "text/html"}],
                                       Data, Req),
    {ok, Req1, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

service_echo(_Conn, init, state)        -> {ok, state};
service_echo(Conn, {recv, Data}, state) -> Conn:send(Data);
service_echo(_Conn, {info, _Info}, state) -> {ok, state};
service_echo(_Conn, closed, state)      -> {ok, state}.
