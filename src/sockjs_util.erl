-module(sockjs_util).

-export([rand32/0]).
-export([encode_frame/1]).

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

-spec encode_frame({open}) -> iodata();
                  ({close, {non_neg_integer(), string()}}) -> iodata();
                  ({data, list(string())}) -> iodata().
encode_frame({open}) ->
    <<"o">>;
encode_frame({close, {Code, Reason}}) ->
    sockjs_json:encode(<<"c">>, [Code, list_to_binary(Reason)]);
encode_frame({data, L}) ->
    sockjs_json:encode(<<"a">>, [D || {data, D} <- L]).
