-module(sockjs_cowboy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init_state/3]).

%% Cowboy http callbacks
-export([init/3, handle/2, terminate/2]).

%% Cowboy ws callbacks
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

init_state(Prefix, ServiceCallback, Options) ->
    Url = proplists:get_value("url", Options,
                              "http://cdn.sockjs.org/sockjs-0.2.js"),
    #state{prefix = binary_to_list(Prefix),
           service_callback = ServiceCallback,
           url = Url,
           websocket = proplists:get_value(websocket, Options, true),
           cookie_needed = proplists:get_value(cookie_needed, Options, false)
          }.

%% --------------------------------------------------------------------------

init({_Any, http}, Req, State) ->
    case sockjs_handler:is_valid_ws(State, {cowboy, Req}) of
        {true, {cowboy, _Req1}} ->
            {upgrade, protocol, cowboy_http_websocket};
        {false, {cowboy, Req1}} ->
            {ok, Req1, State}
    end.

handle(Req, State) ->
    {Method, Req1} = sockjs_http:method({cowboy, Req}),
    {Path, Req2} = sockjs_http:path(Req1),
    io:format("~s ~s ~n", [Method, Path]),
    {cowboy, Req3} = sockjs_handler:handle_req(State, Req2),
    {ok, Req3, State}.

terminate(_Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

websocket_init(_TransportName, Req, State) ->
    {ok, Req, State}.

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

