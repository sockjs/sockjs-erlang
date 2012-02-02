-module(sockjs_util).

-export([rand32/0]).

%% --------------------------------------------------------------------------

-spec rand32() -> non_neg_integer().
rand32() ->
    case get(random_seeded) of
        undefined ->
            {MegaSecs, Secs, MicroSecs} = now(),
            random:seed(MegaSecs, Secs, MicroSecs),
            put(random_seeded, true);
        _Else ->
            ok
    end,
    random:uniform(erlang:trunc(math:pow(2,32)))-1.
