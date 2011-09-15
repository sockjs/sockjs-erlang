-module(sockjs_test).
-export([start/0]).

start() ->
    Port = 8080,
    {ok, _} = misultin:start_link([{loop,        fun loop/1},
                                   {ws_loop,     fun ws_loop/1},
                                   {ws_autoexit, false},
                                   {port,        Port}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    receive
        _ -> ok
    end.

loop(Req) ->
    try
        {abs_path, Path0} = Req:get(uri),
        Path = clean_path(Path0),
        case {Path, lists:reverse(Path)} of
            {_, "lmth." ++ _}   -> static(Req, Path);
            {"static/" ++ _, _} -> static(Req, Path);
            {"lib/" ++ _, _}    -> static(Req, Path);
            {"config.js", _}    -> config_js(Req);
            {_, _}              -> io:format("~s ~s~n",
                                             [Req:get(method), Path]),
                                   Req:respond(404, [], "404")
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
                "var client_opts = {\"url\":\"http://localhost:8080\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true, \"chunking\":true}};").

ws_loop(Ws) ->
    Path = clean_path(Ws:get(path)),
    io:format("~s ~s~n", ["WS", Path]),
    Fun = dispatcher(string:tokens(Path, "/")),
    sockjs_conn_ws:loop(Ws, Fun).

dispatcher([Prefix, _Server, _Session, _Protocol]) ->
    case proplists:get_value(list_to_atom(Prefix), dispatcher()) of
        undefined -> exit({unknown, Prefix});
        Fun       -> Fun
    end.

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

%% --------------------------------------------------------------------------

dispatcher() ->
    [{echo,    fun test_echo/2},
     {close,   fun test_close/2},
     {amplify, fun test_amplify/2}].

test_echo(Conn, {recv, Data}) -> Conn:send(Data);
test_echo(_Conn, _)           -> ok.

test_close(Conn, _) ->
    Conn:close(3000, "Go away!").

test_amplify(Conn, {recv, Data}) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    Conn:send(string:copies("x", round(math:pow(2, N))));
test_amplify(_Conn, _) ->
    ok.
