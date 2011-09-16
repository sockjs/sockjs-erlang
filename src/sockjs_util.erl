-module(sockjs_util).

-export([enc/1, enc/2]).

enc(Thing) ->
    iolist_to_binary(mochijson2:encode(Thing)).

enc(Prefix, Thing) ->
    iolist_to_binary([Prefix, mochijson2:encode(Thing)]).
