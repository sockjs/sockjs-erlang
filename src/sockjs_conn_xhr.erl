-module(sockjs_conn_xhr).

-behaviour(sockjs_conn).

-include("sockjs.hrl").

-export([open/1, send/2, close/3]).
-export([handle_req/5]).

%% TODO why are these here?

open({?MODULE, SessionId}) ->
    enqueue(open, SessionId).

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

close(_Code, _Reason, {?MODULE, _Ws}) ->
    %% TODO why not go crazy and support this?
    exit(bang).

%% --------------------------------------------------------------------------

%% _Fun wut?

handle_req(Req, Server, SessionId, xhr, Fun) ->
    Self = self(),
    case sockjs_session:with_sync(
           fun (S = #session{outbound_queue = Q}) ->
                   case queue:len(Q) of
                       0 -> {wait, S#session{response_pid   = Self}};
                       _ -> {Q,    S#session{outbound_queue = queue:new(),
                                             response_pid   = undefined}}
                   end
           end, SessionId) of
        wait -> receive
                    go -> handle_req(Req, Server, SessionId, xhr, Fun)
                after 5000 ->
                        reply(Req, <<"h">>)
                end;
        Q    -> reply(Req, encode_list(queue:to_list(Q)))
    end.

reply(Req, Body) ->
    Req:ok([], <<Body/binary, $\n>>).

encode_list([open]) ->
    <<"o">>;
encode_list(L) ->
    sockjs_util:enc("a", [D || {data, D} <- L]).
