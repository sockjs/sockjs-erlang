-module(sockjs_erlang_test).
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
                "var client_opts = {\"url\":\"http://localhost:8080\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true}};").

ws_loop(Ws) ->
    Path = clean_path(Ws:get(path)),
    io:format("~s ~s~n", ["WS", Path]),
    Fun = dispatcher(string:tokens(Path, "/")),
    do(Ws, Fun(init)),
    Ws:send(["o"]),
    ws_loop(Ws, Fun).

ws_loop(Ws, Fun) ->
    receive
        {browser, Data} ->
            Decoded = mochijson2:decode(Data),
            do(Ws, Fun({recv, Decoded})),
            ws_loop(Ws, Fun);
        closed ->
            closed;
        Msg ->
            do(Ws, Fun({info, Msg})),
            ws_loop(Ws, Fun)
    end.

%% TODO brand new and already wrong
do(Ws, {send, Data}) ->
    Ws:send(["m", enc(Data)]);
do(Ws, {close, {Code, Reason}}) ->
    Ws:send(["c", enc([Code, list_to_binary(Reason)])]),
    exit(normal);
do(_Ws, ignore) -> ok.

enc(Thing) ->
    iolist_to_binary(mochijson2:encode(Thing)).

dispatcher([Prefix, _Server, _Session, _Protocol]) ->
    case proplists:get_value(list_to_atom(Prefix), dispatcher()) of
        undefined -> exit({unknown, Prefix});
        Fun       -> Fun
    end.

dispatcher() ->
    [{echo,    fun test_echo/1},
     {amplify, fun test_amplify/1},
     {close,   fun test_close/1}].

clean_path("/")         -> "index.html";
clean_path("/" ++ Path) -> Path.

%% --------------------------------------------------------------------------

test_echo({recv, Data}) -> {send, Data};
test_echo(_)            -> ignore.

test_amplify({recv, Data}) ->
    N0 = list_to_integer(binary_to_list(Data)),
    N = if N0 > 0 andalso N0 < 19 -> N0;
           true                   -> 1
        end,
    {send, string:copies("x", round(math:pow(2, N)))};
test_amplify(_) ->
    ignore.

test_close(_) ->
    {close, {3000, "Go away!"}}.
