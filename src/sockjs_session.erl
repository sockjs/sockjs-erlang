-module(sockjs_session).

-behaviour(sockjs_sender).
-behaviour(gen_server).

-export([init/0, start_link/2, maybe_create/2, sender/1, reply/2]).

-export([send/2, close/3]).

-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-record(session, {id, outbound_queue = queue:new(), response_pid, receiver,
                  session_timeout, closed = false, close_msg}).
-define(ETS, sockjs_table).

init() ->
    ets:new(?ETS, [public, named_table]).

start_link(SessionId, Receive) ->
    gen_server:start_link(?MODULE, {SessionId, Receive}, []).

maybe_create(dummy, _) ->
    ok;

maybe_create(SessionId, Receive) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> {ok, SPid} = sockjs_session_sup:start_child(
                                      SessionId, Receive),
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

reply(SessionId, Once) ->
    gen_server:call(spid(SessionId), {reply, self(), Once}, infinity).

%% --------------------------------------------------------------------------

pop_from_queue(Q) ->
    {PoppedRev, Rest} = pop_from_queue(any, [], Q),
    {lists:reverse(PoppedRev), Rest}.

pop_from_queue(TypeAcc, Acc, Q) ->
    case queue:peek(Q) of
        {value, {Type, _}} when TypeAcc =:= any orelse TypeAcc =:= Type ->
            {{value, Val}, Q2} = queue:out(Q),
            pop_from_queue(Type, [Val | Acc], Q2);
        _ -> {Acc, Q}
    end.

spid(SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> throw(no_session);
        [{_, SPid}] -> SPid
    end.

maybe_close([{close, _}] = Msg, State = #session{closed = false}) ->
    State#session{closed = true, close_msg = Msg};
maybe_close([{close, _}],       _State) ->
    exit(assertion_failed);
maybe_close(_,                  State) ->
    State.

reply(Reply, Pid, State = #session{response_pid    = undefined,
                                   session_timeout = Ref}) ->
    link(Pid),
    case Ref of
        undefined -> ok;
        _         -> erlang:cancel_timer(Ref)
    end,
    reply(Reply, Pid, State#session{response_pid    = Pid,
                                    session_timeout = undefined});
reply(Reply, Pid, State = #session{response_pid = Pid}) ->
    {reply, Reply, State}.

%% --------------------------------------------------------------------------

init({SessionId, Receive}) ->
    ets:insert(?ETS, {SessionId, self()}),
    process_flag(trap_exit, true),
    {ok, #session{id = SessionId, receiver = Receive}}.

%% For non-streaming transports we want to send a closed message every time
%% we are asked - for streaming transports we only want to send it once.
handle_call({reply, Pid, true}, _From, State = #session{closed    = true,
                                                        close_msg = Msg}) ->
    reply(sockjs_util:encode_list(Msg), Pid, State);

handle_call({reply, Pid, _Once}, _From, State = #session{response_pid   = RPid,
                                                         outbound_queue = Q}) ->
    case {pop_from_queue(Q), RPid} of
        {{[], _}, P} when P =:= undefined orelse P =:= Pid ->
            reply(wait, Pid, State);
        {{[], _}, _} ->
            %% don't use reply(), this shouldn't touch the session lifetime
            {reply, session_in_use, State};
        {{Popped, Rest}, _} ->
            State1 = maybe_close(Popped, State),
            reply(sockjs_util:encode_list(Popped), Pid,
                  State1#session{outbound_queue = Rest})
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

handle_info({'EXIT', Pid, _Reason},
            State = #session{response_pid = Pid}) ->
    {ok, CloseTime} = application:get_env(sockjs, session_close_ms),
    Ref = erlang:send_after(CloseTime, self(), session_timeout),
    {noreply, State#session{response_pid    = undefined,
                            session_timeout = Ref}};

handle_info(session_timeout, State = #session{response_pid = undefined}) ->
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, #session{id       = SessionId,
                            receiver = Receive}) ->
    Receive({?MODULE, SessionId}, closed),
    ets:delete(?ETS, SessionId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

