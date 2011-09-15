-module(sockjs_util).

-export([with_session/2, enc/1, enc/2]).

-include("sockjs.hrl").

with_session(Fun, SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        []       -> exit(no_session);
        [{_, S}] -> case Fun(S) of
                        S2 = #session{} -> ets:insert(?ETS, {SessionId, S2}),
                                           S2;
                        R               -> R
                    end
    end.

enc(Thing) ->
    iolist_to_binary(mochijson2:encode(Thing)).

enc(Prefix, Thing) ->
    iolist_to_binary([Prefix, mochijson2:encode(Thing)]).
