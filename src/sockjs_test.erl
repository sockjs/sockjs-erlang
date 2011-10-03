-module(sockjs_test).
-export([start/0]).

start() ->
    Port = 8080,
    application:start(sockjs),
    {ok, _} = misultin:start_link([{loop,        fun loop/1},
                                   {ws_loop,     fun ws_loop/1},
                                   {ws_autoexit, false},
                                   {port,        Port}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    test_broadcast(start),
    receive
        _ -> ok
    end.

loop(Req) ->
    try
        {abs_path, Path0} = Req:get(uri),
        Path = clean_path(Path0),
        case sockjs_filters:handle_req(Req, Path, dispatcher()) of
            nomatch -> case Path of
                           "config.js" -> config_js(Req);
                           _           -> static(Req, Path)
                       end;
            _       -> ok
        end
    catch A:B ->
            io:format("~s ~p ~p~n", [A, B, erlang:get_stacktrace()]),
            Req:respond(500, [], "500")
    end.

static(Req, Path) ->
    %% TODO unsafe
    Req:file(filename:join([module_path(), "priv/www", Path])).

module_path() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

config_js(Req) ->
    %% TODO parse the file? Good luck, it's JS not JSON.
    Req:respond(200, [{"content-type", "application/javascript"}],
                "var client_opts = {\"url\":\"http://localhost:8080\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true}};").

ws_loop(Ws) ->
    Path = clean_path(Ws:get(path)),
    %%io:format("~s ~s~n", ["WS", Path]),
    {Fun, _, _, _} = sockjs_filters:dispatch('GET', Path, dispatcher()),
    sockjs_ws:loop(Ws, Fun).

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

%% --------------------------------------------------------------------------

dispatcher() ->
    [{echo,      fun test_echo/2},
     {close,     fun test_close/2},
     {amplify,   fun test_amplify/2},
     {broadcast, fun test_broadcast/2}].

test_echo(Conn, {recv, Data}) -> Conn:send(Data);
test_echo(_Conn, _)           -> ok.

test_close(Conn, _) ->
    Conn:close(3000, "Go away!").

test_amplify(Conn, {recv, Data}) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    Conn:send(list_to_binary(string:copies("x", round(math:pow(2, N)))));
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
    ets:foldl(fun({Conn}, _Acc) -> Conn:send(Data) end, [], broadcast_table),
    ok.
