-module(sockjs_json).

-export([encode/1, decode/1]).

%% --------------------------------------------------------------------------

-spec encode(any()) -> iodata().
encode(Thing) ->
    mochijson2:encode(Thing).

-spec decode(iodata()) -> {ok, any()} | {error, any()}.
decode(Encoded) ->
    try mochijson2:decode(Encoded) of
        V -> {ok, V}
    catch
        E -> {error, E}
    end.
