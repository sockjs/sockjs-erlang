-module(sockjs_util).

-export([enc/1, enc/2, encode_list/1]).

encode_list([{close, {Code, Reason}}]) ->
    %% TODO shut down!
    sockjs_util:enc("c", [Code, list_to_binary(Reason)]);
encode_list([{open, _}]) ->
    <<"o">>;
encode_list(L) ->
    sockjs_util:enc("a", [D || {data, D} <- L]).

enc(Thing) ->
    iolist_to_binary(mochijson2:encode(Thing)).

enc(Prefix, Thing) ->
    iolist_to_binary([Prefix, mochijson2:encode(Thing)]).
