-module(sockjs_cowboy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

%% Cowboy http callbacks
-export([init/3, handle/2, terminate/2]).

%% Cowboy ws callbacks
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

init({_Any, http}, Req, Service) ->
    case sockjs_handler:is_valid_ws(Service, {cowboy, Req}) of
        {true, {cowboy, _Req1}, _Reason} ->
            {upgrade, protocol, cowboy_http_websocket};
        {false, {cowboy, Req1}, _Reason} ->
            {ok, Req1, Service}
    end.

handle(Req, Service) ->
    {Method, Req1} = sockjs_http:method({cowboy, Req}),
    {Path, Req2} = sockjs_http:path(Req1),
    io:format("~s ~s~n", [Method, Path]),
    {cowboy, Req3} = sockjs_handler:handle_req(Service, Req2),
    {ok, Req3, Service}.

terminate(_Req, _Service) ->
    ok.

%% --------------------------------------------------------------------------
%% TODO: heartbeats
%% TODO: infinity as delay

websocket_init(_TransportName, Req, Service) ->
    SessionPid = sockjs_session:maybe_create(undefined, Service#service{
                                                          disconnect_delay=100}),
    {RawWebsocket, {cowboy, Req2}} =
        case sockjs_handler:get_action(Service, {cowboy, Req}) of
            {{match, WS}, Req1} when WS =:= websocket orelse
                                     WS =:= rawwebsocket ->
                {WS, Req1}
        end,
    self() ! go,
    {ok, Req2, {RawWebsocket, SessionPid}}.

websocket_handle({text, Data}, Req, {RawWebsocket, SessionPid} = S) ->
    case sockjs_ws_handler:received(RawWebsocket, SessionPid, Data) of
        ok       -> {ok, Req, S};
        shutdown -> {shutdown, Req, S}
    end;
websocket_handle(_Unknown, Req, S) ->
    {shutdown, Req, S}.

websocket_info(go, Req, {RawWebsocket, SessionPid} = S) ->
    io:format("~p ~p~n", [RawWebsocket, SessionPid]),
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait ->
            {ok, Req, S};
        {ok, Data} ->
            self() ! go,
            {reply, {text, Data}, Req, S};
        {close, <<>>} ->
            {shutdown, Req, S};
        {close, Data} ->
            self() ! shutdown,
            {reply, {text, Data}, Req, S}
    end;
websocket_info(shutdown, Req, S) ->
    {shutdown, Req, S}.

websocket_terminate(_Reason, _Req, _S) ->
    ok.
