-module(sockjs_transport).

-export([init/0, handle_req/3, dispatch/2]).

-include("sockjs.hrl").

init() ->
    ets:new(?ETS, [public, named_table]).

handle_req(Req, Path, Dispatcher) ->
    io:format("~s ~s~n", [Req:get(method), Path]),
    {Fun, Server, SessionId, Transport0} = dispatch(Path, Dispatcher),
    Transport = list_to_atom(Transport0),
    %% TODO there is some confusion between transport and endpoint here.
    Module = case Transport of
                 xhr_send -> sockjs_conn_xhr_send;
                 xhr      -> sockjs_conn_xhr;
                 _        -> exit({unknown_transport, Transport})
             end,
    case ets:lookup(?ETS, SessionId) of
        []       -> S = #session{id       = SessionId,
                                 receiver = {Module, SessionId}},
                    ets:insert(?ETS, {SessionId, S}),
                    Conn = {Module, SessionId},
                    Conn:open(),
                    Fun({Module, Conn}, init),
                    S;
        [{_, S}] -> S
    end,
    Module:handle_req(Req, Server, SessionId, Transport, Fun).

dispatch(Path, Dispatcher) ->
    [Prefix, Server, SessionId, Transport] = string:tokens(Path, "/"),
    case proplists:get_value(list_to_atom(Prefix), Dispatcher) of
        undefined -> exit({unknown, Prefix});
        Fun       -> {Fun, Server, SessionId, Transport}
    end.
