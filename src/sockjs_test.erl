-module(sockjs_test).
-export([start/0]).

start() ->
    Dispatch =
        [{echo,      fun test_echo/2},
         {close,     fun test_close/2},
         {amplify,   fun test_amplify/2},
         {broadcast, fun test_broadcast/2}],
    Port = 8080,
    application:start(sockjs),
    {ok, HttpImpl} = application:get_env(sockjs, http_impl),
    case HttpImpl of
        misultin ->
            {Loop, WsLoop} = sockjs_misultin:init_state(fun fallback/1,
                                                        Dispatch),
            {ok, _} = misultin:start_link([{loop,        Loop},
                                           {ws_loop,     WsLoop},
                                           {ws_autoexit, false},
                                           {port,        Port}]);
        cowboy ->
            application:start(cowboy),
            Routes = [{'_', [{'_', sockjs_cowboy,
                                sockjs_cowboy:init_state(fun fallback/1,
                                                         Dispatch)}]}],
            cowboy:start_listener(http, 100,
                                  cowboy_tcp_transport, [{port,     Port}],
                                  cowboy_http_protocol, [{dispatch, Routes}])
    end,
    error_logger:info_msg("~nRunning on port ~p~n~n", [Port]),
    test_broadcast(start),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

fallback(Req) ->
    {Path0, Req1} = sockjs:path(Req),
    Path = clean_path(Path0),
    case Path of
        "config.js" -> config_js(Req1);
        _           -> static(Req1, Path)
    end.

static(Req, Path) ->
    %% TODO unsafe
    LocalPath = filename:join([module_path(), "priv/www", Path]),
    case file:read_file(LocalPath) of
        {ok, Contents} ->
            sockjs:respond(200, [], Contents, Req);
        {error, _} ->
            sockjs:respond(404, [], "", Req)
    end.

module_path() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

config_js(Req) ->
    %% TODO parse the file? Good luck, it's JS not JSON.
    sockjs:respond(
      200, [{"content-type", "application/javascript"}],
      "var client_opts = {\"url\":\"http://localhost:8080\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true}};", Req).

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

%% --------------------------------------------------------------------------

test_echo(Conn, {recv, Data}) -> sockjs:send(Data, Conn);
test_echo(_Conn, _)           -> ok.

test_close(Conn, _) ->
    sockjs:close(3000, "Go away!", Conn).

test_amplify(Conn, {recv, Data}) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    sockjs:send(list_to_binary(string:copies("x", round(math:pow(2, N)))), Conn);
test_amplify(_Conn, _) ->
    ok.


test_broadcast(start) ->
    ets:new(broadcast_table, [public, named_table]),
    ok.
test_broadcast(Conn, init) ->
    true = ets:insert(broadcast_table, {Conn}),
    ok;
test_broadcast(Conn, closed) ->
    true = ets:delete_object(broadcast_table, {Conn}),
    ok;
test_broadcast(_Conn, {recv, Data}) ->
    ets:foldl(fun({Conn}, _Acc) -> sockjs:send(Data, Conn) end, [], broadcast_table),
    ok.
