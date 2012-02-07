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
    SessionPid = sockjs_session:maybe_create(undefined,Service#service{
                                                         disconnect_delay=100}),
    self() ! go,
    {ok, Req, {SessionPid, Service}}.

%% Ignore empty
websocket_handle({text, <<>>}, Req, S) ->
    {ok, Req, S};
websocket_handle({text, Data}, Req,
                 {SessionPid, _Service} = S) ->
    case sockjs_json:decode(Data) of
        {ok, Msg} when is_binary(Msg) ->
            sockjs_session:received([Msg], SessionPid),
            {ok, Req, S};
        {ok, Messages} when is_list(Messages) ->
            sockjs_session:received(Messages, SessionPid),
            {ok, Req, S};
        _Else ->
            {shutdown, Req, S}
    end;
websocket_handle(_Unknown, Req, S) ->
    {shutdown, Req, S}.

websocket_info(go, Req, {SessionPid, _Service} = S) ->
    case sockjs_session:reply(SessionPid) of
        {ok, Frame} ->
            self() ! go,
            Frame1 = sockjs_util:encode_frame(Frame),
            {reply, {text, iolist_to_binary(Frame1)}, Req, S};
        wait ->
            {ok, Req, S};
        {close, Frame} ->
            self() ! shutdown,
            Frame1 = sockjs_util:encode_frame(Frame),
            {reply, {text, iolist_to_binary(Frame1)}, Req, S}
    end;
websocket_info(shutdown, Req, S) ->
    {shutdown, Req, S}.

websocket_terminate(_Reason, _Req, _S) ->
    ok.
