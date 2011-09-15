-module(sockjs_conn_xhr).

-behaviour(sockjs_conn).

-include("sockjs.hrl").

-export([open/1, send/2, close/3]).
-export([handle_req/5]).

%% TODO this is buggy - nothing stops simultaneous modification of a
%% session by two processes. And in Erlang too. How embarassing.

%% TODO why are these here?

open({?MODULE, SessionId}) ->
    enqueue(open, SessionId).

send(Data, {?MODULE, SessionId}) ->
    enqueue({data, Data}, SessionId).

enqueue(Cmd, SessionId) ->
    case sockjs_util:with_session(
           fun (Session = #session{outbound_queue = Q}) ->
                   Session#session{outbound_queue = queue:in(Cmd, Q)}
           end, SessionId) of
        #session{response_pid = P} when is_pid(P) -> P ! go;
        _ -> ok
    end.

close(_Code, _Reason, {?MODULE, _Ws}) ->
    %% TODO why not go crazy and support this?
    exit(bang).

%% --------------------------------------------------------------------------

%% _Fun wut?

handle_req(Req, Server, SessionId, xhr, Fun) ->
    case sockjs_util:with_session(
           fun (Session = #session{outbound_queue = Q}) ->
                   case queue:len(Q) of
                       0 -> Session#session{response_pid = self()};
                       _ -> R = encode_list(queue:to_list(Q)),
                            reply(Req, R),
                            Session#session{outbound_queue = queue:new(),
                                            response_pid   = undefined}
                   end
           end, SessionId) of
        #session{response_pid = P} when is_pid(P) ->
            receive
                go -> handle_req(Req, Server, SessionId, xhr, Fun)
            after 5000 ->
                    reply(Req, <<"h">>)
            end;
        _ ->
            ok
    end.

reply(Req, Body) ->
    Req:ok([], <<Body/binary, $\n>>).

encode_list([open]) ->
    <<"o">>;
encode_list(L) ->
    sockjs_util:enc("a", [D || {data, D} <- L]).
