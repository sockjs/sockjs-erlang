#!/usr/bin/env escript
%%! -smp disable +A1 +K true -pa ebin deps/misultin/ebin -input
-module(misultin_echo).
-mode(compile).

-export([main/1]).


main(_) ->
    Port = 8081,
    application:start(sockjs),

    SockjsState = sockjs_handler:init_state(
                    <<"/echo">>, fun service_echo/2, []),

    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    misultin:start_link(
      [{port, Port},
       {autoexit, false},
       {ws_autoexit, false},
       {loop,    fun (Req) -> handle_http(Req, SockjsState) end},
       {ws_loop, fun (Req) -> handle_ws(Req, SockjsState) end}]),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

handle_http(Req, SockjsState) ->
    case Req:resource([]) of
        ["echo" | _T] ->
            sockjs_handler:handle_req(SockjsState, {misultin, Req});
        _Any ->
            {ok, Data} = file:read_file("./examples/echo.html"),
            Req:respond(200, [{"Content-Type", "text/html"}], Data)
    end.

handle_ws(Req, SockjsState) ->
    case string:tokens(Req:get(path), "/") of
        ["echo" | _T] ->
            sockjs_misultin_handler:handle_ws(SockjsState, Req);
        _Else ->
            %% abort any other ws request
            normal
    end.

%% --------------------------------------------------------------------------

service_echo(Conn, {recv, Data}) -> sockjs:send(Data, Conn);
service_echo(_Conn, _)           -> ok.
