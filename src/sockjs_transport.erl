-module(sockjs_transport).

-export([handle_req/3, dispatch/2]).

handle_req(Req, Path, Dispatcher) ->
    io:format("~s ~s~n", [Req:get(method), Path]),
    {Fun, Server, SessionId, Filters} = dispatch(Path, Dispatcher),
    sockjs_session:maybe_create(SessionId, Fun),
    [sockjs_filters:F(Req, Server, SessionId, F, Fun) || F <- Filters].

dispatch(Path, Dispatcher) ->
    [Prefix, Server, SessionId, Endpoint] = string:tokens(Path, "/"),
    case proplists:get_value(list_to_atom(Prefix), Dispatcher) of
        undefined -> exit({unknown, Prefix});
        Fun       -> {Fun, Server, SessionId, sockjs_filters:filters(Endpoint)}
    end.
