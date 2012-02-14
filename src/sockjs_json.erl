-module(sockjs_json).

-export([encode/1, decode/1]).

%% --------------------------------------------------------------------------

-spec encode(any()) -> iodata().
encode(Thing) ->
    encode(encoder(), Thing).

-spec decode(iodata()) -> {ok, any()} | {error, any()}.
decode(Encoded) ->
    decode(encoder(), Encoded).


encoder() ->
    case get(sockjs_json_impl) of
        undefined ->
            E = case application:get_env(sockjs, json_impl) of
                    {ok, Encoder} -> Encoder;
                    undefined     -> jiffy %mochijson2_fork
                end,
            undefined = put(sockjs_json_impl, E),
            E;
        E -> E
    end.

%% -----

-spec encode(mochijson2_fork | jiffy, any()) -> iodata().
encode(mochijson2_fork, Thing) ->
    mochijson2_fork:encode(Thing);

encode(jiffy, Thing) ->
    jiffy:encode(Thing).

%% -----

-spec decode(mochijson2_fork | jiffy, iodata()) -> {ok, any()} | {error, any()}.
decode(mochijson2_fork, Encoded) ->
    try mochijson2_fork:decode(Encoded) of
        V -> {ok, V}
    catch
        E -> {error, E}
    end;

decode(jiffy, Encoded) ->
    io:format("Encoded: ~p~n", [Encoded]),
    try jiffy:decode(Encoded) of
        V -> {ok, V}
    catch
        E -> E
    end.
