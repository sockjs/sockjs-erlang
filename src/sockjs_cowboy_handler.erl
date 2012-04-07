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
    {cowboy, Req3} = sockjs_handler:handle_req(Service, {cowboy, Req}),
    {ok, Req3, Service}.

terminate(_Req, _Service) ->
    ok.

%% --------------------------------------------------------------------------

websocket_init(_TransportName, Req, Service = #service{logger = Logger, hibernate = Hibernate}) ->
    Req0 = Logger(Service, {cowboy, Req}, websocket),

    {Info, Req1} = sockjs_handler:extract_info(Req0),
    SessionPid = sockjs_session:maybe_create(undefined, Service, Info),
    {RawWebsocket, {cowboy, Req3}} =
        case sockjs_handler:get_action(Service, Req1) of
            {{match, WS}, Req2} when WS =:= websocket orelse
                                     WS =:= rawwebsocket ->
                {WS, Req2}
        end,
    self() ! go,
    case Hibernate of
        true -> {ok, Req3, {RawWebsocket, SessionPid, Hibernate}, hibernate};
        _    -> {ok, Req3, {RawWebsocket, SessionPid, Hibernate}}
    end.

websocket_handle({text, Data}, Req, {RawWebsocket, SessionPid, Hibernate} = S) ->
    case sockjs_ws_handler:received(RawWebsocket, SessionPid, Data) of
        ok       ->
                    case Hibernate of
                        true -> {ok, Req, S, hibernate};
                        _ ->    {ok, Req, S}
                    end;
        shutdown -> {shutdown, Req, S}
    end;
websocket_handle(_Unknown, Req, S) ->
    {shutdown, Req, S}.

websocket_info(go, Req, {RawWebsocket, SessionPid, Hibernate} = S) ->
    case sockjs_ws_handler:reply(RawWebsocket, SessionPid) of
        wait          ->
                         case Hibernate of
                             true -> {ok, Req, S, hibernate};
                             _    -> {ok, Req, S}
                         end;
        {ok, Data}    -> self() ! go,
                         case Hibernate of
                             true -> {reply, {text, Data}, Req, S, hibernate};
                             _    -> {reply, {text, Data}, Req, S}
                         end;
        {close, <<>>} -> {shutdown, Req, S};
        {close, Data} -> self() ! shutdown,
                         {reply, {text, Data}, Req, S}
    end;
websocket_info(shutdown, Req, S) ->
    {shutdown, Req, S}.

websocket_terminate(_Reason, _Req, {RawWebsocket, SessionPid, _Hibernate}) ->
    sockjs_ws_handler:close(RawWebsocket, SessionPid),
    ok.
