-module(sockjs_session).

-behaviour(sockjs_sender).
-behaviour(gen_server).

-export([init/0, start_link/1, maybe_create/2, sender/1, reply/1]).

-export([send/2, close/3]).

-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-record(session, {id, outbound_queue = queue:new(), response_pid}).
-define(ETS, sockjs_table).

init() ->
    ets:new(?ETS, [public, named_table]).

start_link(SessionId) ->
    gen_server:start_link(?MODULE, SessionId, []).

maybe_create(SessionId, Receive) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> {ok, SPid} = sockjs_session_sup:start_child(SessionId),
                       ets:insert(?ETS, {SessionId, SPid}),
                       enqueue({open, nil}, SessionId),
                       Receive({?MODULE, SessionId}, init),
                       SPid;
        [{_, SPid}] -> SPid
    end.

send(Data, {?MODULE, SessionId}) ->
    enqueue({data, Data}, SessionId).

close(Code, Reason, {?MODULE, SessionId}) ->
    enqueue({close, {Code, Reason}}, SessionId).

enqueue(Cmd, SessionId) ->
    gen_server:cast(spid(SessionId), {enqueue, Cmd}).

sender(SessionId) -> {?MODULE, SessionId}.

reply(SessionId) ->
    gen_server:call(spid(SessionId), {reply, self()}, infinity).

%% --------------------------------------------------------------------------

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

pop_from_queue(TypeAcc, Acc, Q) ->
    case queue:peek(Q) of
        empty ->
            {Acc, Q};
        {value, {Type, _}} ->
            if TypeAcc =:= any orelse TypeAcc =:= Type ->
                    {{value, Val}, Q2} = queue:out(Q),
                    %% Serve close forever
                    case Type of
                        close -> {[Val | Acc], Q};
                        _     -> pop_from_queue(Type, [Val | Acc], Q2)
                    end;
               true ->
                    {Acc, Q}
            end
    end.

spid(SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> throw(no_session);
        [{_, SPid}] -> SPid
    end.

%% --------------------------------------------------------------------------

init(SessionId) ->
    {ok, #session{id = SessionId}}.

handle_call({reply, Pid}, _From, State = #session{outbound_queue = Q}) ->
    case pop_from_queue(Q) of
        {[], _} ->
            {reply, wait, State#session{response_pid = Pid}};
        {Popped, Rest} ->
            {reply, encode_list(Popped),
             State#session{outbound_queue = Rest,
                           response_pid   = undefined}}
    end;

handle_call(Request, _From, State) ->
    {stop, {odd_request, Request}, State}.

handle_cast({enqueue, Cmd}, State = #session{outbound_queue = Q,
                                             response_pid   = P}) ->
    if is_pid(P) -> P ! go;
       true      -> ok
    end,
    {noreply, State#session{outbound_queue = queue:in(Cmd, Q)}};

handle_cast(Cast, State) ->
    {stop, {odd_cast, Cast}, State}.

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

