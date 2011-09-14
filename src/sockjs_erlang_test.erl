-module(sockjs_erlang_test).
-export([start/0]).

start() ->
    Port = 8080,
    {ok, _} = misultin:start_link([{loop, fun loop/1},
                                   {port, Port}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    receive
        _ -> ok
    end.

loop(Req) ->
    try
        {abs_path, Path0} = Req:get(uri),
        "/" ++ Path = case Path0 of
                          "/" -> "/index.html";
                          P   -> P
                      end,
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
