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

websocket_init(_TransportName, Req, Service) ->
    {ok, Req, Service}.

websocket_handle(_Data, Req, Service) ->
    {ok, Req, Service}.

websocket_info(_Info, Req, Service) ->
    {ok, Req, Service}.

websocket_terminate(_Reason, _Req, _Service) ->
    ok.
