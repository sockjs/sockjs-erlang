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
        {true, {cowboy, _Req1}} ->
            {upgrade, protocol, cowboy_http_websocket};
        {false, {cowboy, Req1}} ->
            {ok, Req1, Service}
    end.

handle(Req, Service) ->
    {Method, Req1} = sockjs_http:method({cowboy, Req}),
    {Path, Req2} = sockjs_http:path(Req1),
    io:format("~s ~s ~n", [Method, Path]),
    {cowboy, Req3} = sockjs_handler:handle_req(Service, Req2),
    {ok, Req3, Service}.

terminate(_Req, _Service) ->
    ok.

%% --------------------------------------------------------------------------
%% TODO: heartbeats

websocket_init(_TransportName, Req, Service) ->
    SessionPid = sockjs_session:maybe_create(undefined,Service#service{
                                                         disconnect_delay=0}),
    self() ! go,
    {ok, Req, {SessionPid, Service}}.

websocket_handle({text, <<>>}, Req, {SessionPid, _Service} = S) ->
    {ok, Req, S};
websocket_handle({text, <<$", Rest/binary>>}, Req,
                 {SessionPid, _Service} = S) ->
    L = size(Rest) - 1,
    case Rest of
        <<Data:L/binary, $">> ->
            sockjs_session:received(Data, SessionPid),
            {ok, Req, S};
        _Else ->
            {shutdown, Req, S}
    end;
websocket_handle({text, Data = <<$[, _Rest/binary>>}, Req,
                 {SessionPid, _Service} = S) ->
    case sockjs_json:decode(Data) of
        {ok, Messages} when is_list(Messages) ->
            [sockjs_session:received(Message, SessionPid) ||
                Message <- Messages],
            {ok, Req, S};
        _Else ->
            {shutdown, Req, S}
    end.

websocket_info(go, Req, {SessionPid, _Service} = S) ->
    case sockjs_session:reply(SessionPid) of
        {ok, Data} ->
            self() ! go,
            {reply, {text, iolist_to_binary(Data)}, Req, S};
        wait ->
            {ok, Req, S};
        {close, Data} ->
            self() ! shutdown,
            {reply, {text, iolist_to_binary(Data)}, Req, S}
    end;
websocket_info(shutdown, Req, {SessionPid, _Service} = S) ->
    {shutdown, Req, S}.

websocket_terminate(_Reason, _Req, {_SessionPid, _Service}) ->
    ok.
