-module(sockjs_session).

-behaviour(sockjs_sender).
-behaviour(gen_server).

-export([init/0, start_link/2]).
-export([maybe_create/2, reply/1, received/2]).
-export([send/2, close/3]).


-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-record(session, {id,
                  outbound_queue = queue:new(),
                  response_pid,
                  receiver,
                  session_timeout,
                  disconnect_delay,
                  ready_state = connecting,
                  close_msg,
                  callback,
                  handle :: handle()}).
-define(ETS, sockjs_table).

-type(handle() :: {?MODULE, {binary(), pid()}}).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec init() -> ok.
init() ->
    ets:new(?ETS, [public, named_table]),
    ok.

-spec start_link(session(), service()) -> {ok, pid()}.
start_link(SessionId, Service) ->
    gen_server:start_link(?MODULE, {SessionId, Service}, []).

-spec maybe_create(session(), service()) -> ok.
maybe_create(SessionId, Service) ->
    case ets:lookup(?ETS, SessionId) of
        []           -> {ok, _SPid} = sockjs_session_sup:start_child(
                                        SessionId, Service),
                        ok;
        [{_, _SPid}] -> ok
    end.

-spec received(iodata(), session()) -> ok.
received(Data, SessionId) ->
    case gen_server:call(spid(SessionId), {received, Data}, infinity) of
        ok -> ok;
        error -> %% TODO: should we respond 404 when session is closed?
            throw(no_session)
    end.

-spec send(iodata(), handle()) -> ok.
send(Data, {?MODULE, {_, SPid}}) ->
    gen_server:cast(SPid, {send, Data}),
    ok.

-spec close(non_neg_integer(), string(), handle()) -> ok.
close(Code, Reason, {?MODULE, {_, SPid}}) ->
    gen_server:cast(SPid, {close, Code, Reason}),
    ok.

reply(SessionId) ->
    gen_server:call(spid(SessionId), {reply, self()}, infinity).

%% --------------------------------------------------------------------------

spid(SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> throw(no_session);
        [{_, SPid}] -> SPid
    end.

%% pop_type_from_queue(Q) ->
%%     {PoppedRev, Rest} = pop_type_from_queue(any, [], Q),
%%     {lists:reverse(PoppedRev), Rest}.

%% pop_type_from_queue(TypeAcc, Acc, Q) ->
%%     case queue:peek(Q) of
%%         {value, {Type, _}} when TypeAcc =:= any orelse TypeAcc =:= Type ->
%%             {{value, Val}, Q2} = queue:out(Q),
%%             pop_type_from_queue(Type, [Val | Acc], Q2);
%%         _ -> {Acc, Q}
%%     end.


%% maybe_close([{close, _}] = Msg, State = #session{closed = false}) ->
%%     State#session{closed = true, close_msg = Msg};
%% maybe_close([{close, _}],       _State) ->
%%     exit(assertion_failed);
%% maybe_close(_,                  State) ->
%%     State.

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

emit(What, #session{callback = Callback,
                    handle = Handle}) ->
    Callback(Handle, What).

%% --------------------------------------------------------------------------

-spec init({session(), service()}) -> {ok, #session{}}.
init({SessionId, #service{callback         = Callback,
                          disconnect_delay = DisconnectDelay}}) ->
    ets:insert(?ETS, {SessionId, self()}),
    process_flag(trap_exit, true),
    {ok, #session{id = SessionId,
                  callback = Callback,
                  disconnect_delay = DisconnectDelay,
                  handle = {?MODULE, {sockjs_util:guid(), self()}}}}.


handle_call({reply, Pid}, _From, State = #session{ready_state = connecting}) ->
    emit(init, State),
    reply(sockjs_util:encode_frame({open, nil}), Pid,
          State#session{ready_state = open});

handle_call({reply, Pid}, _From, State = #session{ready_state = closed,
                                                     close_msg = CloseMsg}) ->
    reply(sockjs_util:encode_frame({close, CloseMsg}), Pid, State);


handle_call({reply, Pid}, _From, State = #session{ready_state = open,
                                                         response_pid = RPid,
                                                         outbound_queue = Q}) ->
    {Messages, Q1} = {queue:to_list(Q), queue:new()},
    case {Messages, RPid} of
        {[], P} when P =:= undefined orelse P =:= Pid ->
            reply(wait, Pid, State);
        {[], _} ->
            %% don't use reply(), this shouldn't touch the session lifetime
            {reply, session_in_use, State};
        {_, _} ->
            reply(sockjs_util:encode_frame({data, Messages}), Pid,
                  State#session{outbound_queue = Q1})
    end;

handle_call({received, Data}, _From, State = #session{ready_state = open}) ->
    emit({recv, iolist_to_binary(Data)}, State),
    {reply, ok, State};

handle_call({received, _Data}, _From, State = #session{ready_state = _Any}) ->
    {reply, error, State};

handle_call(Request, _From, State) ->
    {stop, {odd_request, Request}, State}.


handle_cast({send, Data}, State = #session{outbound_queue = Q,
                                           response_pid   = RPid}) ->
    case RPid of
        undefined -> ok;
        _Else     -> RPid ! go
    end,
    {noreply, State#session{outbound_queue = queue:in(Data, Q)}};

handle_cast({close, Status, Reason},  State = #session{response_pid = RPid}) ->
    case RPid of
        undefined -> ok;
        _Else     -> RPid ! go
    end,
    {noreply, State#session{ready_state = closed,
                            close_msg = {Status, Reason}}};

handle_cast(Cast, State) ->
    {stop, {odd_cast, Cast}, State}.


handle_info({'EXIT', Pid, _Reason},
            State = #session{response_pid = Pid,
                             disconnect_delay = DisconnectDelay}) ->
    Ref = erlang:send_after(DisconnectDelay, self(), session_timeout),
    {noreply, State#session{response_pid    = undefined,
                            session_timeout = Ref}};

handle_info(session_timeout, State = #session{response_pid = undefined}) ->
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.


terminate(Reason, State = #session{id       = SessionId}) ->
    io:format("exit reason ~p ~n", [Reason]),
    emit(closed, State),
    ets:delete(?ETS, SessionId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

