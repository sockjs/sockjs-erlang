-module(sockjs_conn_xhr).

-behaviour(sockjs_conn).

-include("sockjs.hrl").

-export([open/1, send/2, close/3]).
-export([handle_req/5]).

%% TODO why are these here?

open({?MODULE, SessionId}) ->
    enqueue({open, nil}, SessionId).

send(Data, {?MODULE, SessionId}) ->
    enqueue({data, Data}, SessionId).

enqueue(Cmd, SessionId) ->
    sockjs_session:with(fun (S = #session{outbound_queue = Q,
                                          response_pid   = P}) ->
                                if is_pid(P) -> P ! go;
                                   true      -> ok
                                end,
                                S#session{outbound_queue = queue:in(Cmd, Q)}
                        end, SessionId).

close(Code, Reason, {?MODULE, SessionId}) ->
    enqueue({close, {Code, Reason}}, SessionId).

%% --------------------------------------------------------------------------

%% _Fun wut?

handle_req(Req, Server, SessionId, xhr, Fun) ->
    Self = self(),
    case sockjs_session:with_sync(
           fun (S = #session{outbound_queue = Q}) ->
                   case pop_from_queue(Q) of
                       {[], _} ->
                           {wait, S#session{response_pid = Self}};
                       {Popped, Rest} ->
                           {Popped, S#session{outbound_queue = Rest,
                                              response_pid   = undefined}}
                   end
           end, SessionId) of
        wait   -> receive
                      go -> handle_req(Req, Server, SessionId, xhr, Fun)
                  after 5000 ->
                          reply(Req, <<"h">>)
                  end;
        Popped -> reply(Req, encode_list(Popped))
    end.

reply(Req, Body) ->
    Req:ok([], <<Body/binary, $\n>>).

encode_list([{close, {Code, Reason}}]) ->
    %% TODO shut down!
    sockjs_util:enc("c", [Code, list_to_binary(Reason)]);
encode_list([{open, _}]) ->
    <<"o">>;
encode_list(L) ->
    sockjs_util:enc("a", [D || {data, D} <- L]).

pop_from_queue(Q) ->
    {PoppedRev, Rest} = pop_from_queue(any, [], Q),
    {lists:reverse(PoppedRev), Rest}.

pop_from_queue(any, [], Q) ->
    case queue:out(Q) of
        {empty, Q}                     -> {[], Q};
        {{value, Val = {Type, _}}, Q2} -> pop_from_queue(Type, [Val], Q2)
    end;
pop_from_queue(Type, Acc, Q) ->
    case queue:peek(Q) of
        empty              -> {Acc, Q};
        {value, {Type, _}} -> {{value, Val}, Q2} = queue:out(Q),
                              pop_from_queue(Type, [Val | Acc], Q2);
        {value, {_, _}}    -> {Acc, Q}
    end.
