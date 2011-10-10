-module(sockjs_util).

-export([encode/1, encode/2, decode/1, encode_list/1]).
-export([mochijson2/2, eep0018/2]).

encode_list([{close, {Code, Reason}}]) ->
    encode(<<"c">>, [Code, list_to_binary(Reason)]);
encode_list([{open, _}]) ->
    <<"o">>;
encode_list(L) ->
    encode(<<"a">>, [D || {data, D} <- L]).

encode(Prefix, Thing) ->
    JSON = encode(Thing),
    <<Prefix/binary, JSON/binary>>.

encode(Thing) ->
    {ok, Encoder} = application:get_env(sockjs, json_impl),
    sockjs_util:Encoder(Thing, encode).

decode(Thing) ->
    {ok, Encoder} = application:get_env(sockjs, json_impl),
    sockjs_util:Encoder(Thing, decode).

mochijson2(Thing, encode) -> iolist_to_binary(mochijson2:encode(Thing));
mochijson2(JSON,  decode) ->
    try mochijson2:decode(JSON) of
        V -> {ok, V}
    catch
        E -> {error, E}
    end.

eep0018(Thing, encode) -> {ok, JSON}  = json:encode(Thing), JSON;
eep0018(JSON,  decode) -> json:decode(JSON).
