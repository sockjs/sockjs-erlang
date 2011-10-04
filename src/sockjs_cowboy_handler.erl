-module(sockjs_cowboy_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-define(WS_MODULE, sockjs_ws).
-record(state, {self, recv}).

init({tcp, http}, Req, _Opts) ->
    {Upgrade, Req1} = cowboy_http_req:header('Upgrade', Req),
    case Upgrade of
        <<"WebSocket">> -> {upgrade, protocol, cowboy_http_websocket};
        _               -> {ok,      Req1,     undefined_state}
    end.

handle(Req, State) ->
    {Path0, Req1} = cowboy_http_req:raw_path(Req),
    Path = clean_path(binary_to_list(Path0)),
    Req2 = case sockjs_filters:handle_req(
                  Req1, Path, sockjs_test:dispatcher()) of
               nomatch -> case Path of
                              "config.js" -> config_js(Req1);
                              _           -> static(Req1, Path)
                          end;
               R2      -> R2
           end,
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

websocket_init(_TransportName, Req, _Opts) ->
    {Path0, Req1} = cowboy_http_req:raw_path(Req),
    Path = clean_path(binary_to_list(Path0)),
    {Receive, _, _, _} = sockjs_filters:dispatch('GET', Path,
                                                 sockjs_test:dispatcher()),
    Self = {?WS_MODULE, self()},
    self() ! {send, ["o"]},
    Receive(Self, init),
    {ok, Req1, #state{self = Self, recv = Receive}}.

websocket_handle({text, Text}, Req,
                 State = #state{self = Self, recv = Receive}) ->
    Decoded = sockjs_util:decode(Text),
    Receive(Self, {recv, Decoded}),
    {ok, Req, State};

websocket_handle(Data, Req, State) ->
    io:format("Handle ~p~n", [Data]),
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
%% Client closed
websocket_info(closed, Req, State = #state{self = Self, recv = Receive}) ->
    Receive(Self, closed),
    {shutdown, Req, State};
%% Server closed
websocket_info(shutdown, Req, State) ->
    {shutdown, Req, State};
websocket_info(Info, Req, State = #state{self = Self, recv = Receive}) ->
    Receive(Self, {info, Info}),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

static(Req, Path) ->
    %% TODO unsafe
    LocalPath = filename:join([module_path(), "priv/www", Path]),
    case file:read_file(LocalPath) of
        {ok, Contents} ->
            {ok, Req1} = cowboy_http_req:reply(200, [], Contents, Req),
            Req1;
        {error, _} ->
            {ok, Req1} = cowboy_http_req:reply(404, [], "", Req),
            Req1
    end.

module_path() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

config_js(Req) ->
    %% TODO parse the file? Good luck, it's JS not JSON.
    {ok, Req1} = cowboy_http_req:reply(
                   200, [{<<"content-type">>, <<"application/javascript">>}],
                   "var client_opts = {\"url\":\"http://localhost:8080\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true}};", Req),
    Req1.

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

