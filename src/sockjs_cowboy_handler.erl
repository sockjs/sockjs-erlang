-module(sockjs_cowboy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-define(WS_MODULE, sockjs_ws).
-record(state, {handler}).
-record(ws_state, {self, recv}).

init({tcp, http}, Req, {Handler, _WsHandler}) ->
    {Upgrade, Req1} = cowboy_http_req:header('Upgrade', Req),
    case Upgrade of
        <<"WebSocket">> -> {upgrade, protocol, cowboy_http_websocket};
        _               -> {ok, Req1, #state{handler = Handler}}
    end.

handle(Req, State = #state{handler = Handler}) ->
    {ok, Handler(Req), State}.

terminate(_Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

websocket_init(_TransportName, Req, {_Handler, WsHandler}) ->
    {Receive, Req1} = WsHandler(Req),
    Self = {?WS_MODULE, self()},
    self() ! {send, ["o"]},
    Receive(Self, init),
    {ok, Req1, #ws_state{self = Self, recv = Receive}}.

websocket_handle({text, Text}, Req,
                 State = #ws_state{self = Self, recv = Receive}) ->
    Decoded = sockjs_util:decode(Text),
    Receive(Self, {recv, Decoded}),
    {ok, Req, State};

websocket_handle(Data, Req, State) ->
    io:format("Handle ~p~n", [Data]),
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
%% Client closed
websocket_info(closed, Req, State = #ws_state{self = Self, recv = Receive}) ->
    Receive(Self, closed),
    {shutdown, Req, State};
%% Server closed
websocket_info(shutdown, Req, State) ->
    {shutdown, Req, State};
websocket_info(Info, Req, State = #ws_state{self = Self, recv = Receive}) ->
    Receive(Self, {info, Info}),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
