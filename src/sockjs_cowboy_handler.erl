-module(sockjs_cowboy_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

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

ws_loop(Ws) ->
    Path = clean_path(Ws:get(path)),
    %%io:format("~s ~s~n", ["WS", Path]),
    {Fun, _, _, _} = sockjs_filters:dispatch('GET', Path, sockjs_test:dispatcher()),
    sockjs_ws:loop(Ws, Fun).

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

