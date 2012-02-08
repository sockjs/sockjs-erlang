-module(sockjs_session).

-behaviour(sockjs_sender).
-behaviour(gen_server).

-export([init/0, start_link/2]).
-export([maybe_create/2, reply/1, reply/2, received/2]).
-export([send/2, close/3]).


-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-record(session, {id :: session() | undefined,
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

-type(session_or_undefined() :: session() | undefined).
-type(session_or_pid() :: session() | pid()).

%% --------------------------------------------------------------------------

-spec init() -> ok.
init() ->
    _ = ets:new(?ETS, [public, named_table]),
    ok.

-spec start_link(session_or_undefined(), service()) -> {ok, pid()}.
start_link(SessionId, Service) ->
    gen_server:start_link(?MODULE, {SessionId, Service}, []).

-spec maybe_create(session_or_undefined(), service()) -> pid().
maybe_create(SessionId, Service) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> {ok, SPid} = sockjs_session_sup:start_child(
                                      SessionId, Service),
                       SPid;
        [{_, SPid}] -> SPid
    end.


-spec received(list(iodata()), session_or_pid()) -> ok.
received(Messages, SessionPid) when is_pid(SessionPid) ->
    case gen_server:call(SessionPid, {received, Messages}, infinity) of
        ok    -> ok;
        error -> throw(no_session)
                 %% TODO: should we respond 404 when session is closed?
    end;
received(Messages, SessionId) ->
    received(Messages, spid(SessionId)).

-spec send(iodata(), handle()) -> ok.
send(Data, {?MODULE, {_, SPid}}) ->
    gen_server:cast(SPid, {send, Data}),
    ok.

-spec close(non_neg_integer(), string(), handle()) -> ok.
close(Code, Reason, {?MODULE, {_, SPid}}) ->
    gen_server:cast(SPid, {close, Code, Reason}),
    ok.

-spec reply(session_or_pid()) ->
                   wait | session_in_use | {ok | close, {open | close, any()}}.
reply(Session) ->
    reply(Session, true).

-spec reply(session_or_pid(), boolean()) ->
                   wait | session_in_use | {ok | close, {open | close, any()}}.
reply(SessionPid, Multiple) when is_pid(SessionPid) ->
    gen_server:call(SessionPid, {reply, self(), Multiple}, infinity);
reply(SessionId, Multiple) ->
    reply(spid(SessionId), Multiple).


%% --------------------------------------------------------------------------

spid(SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> throw(no_session);
        [{_, SPid}] -> SPid
    end.

mark_waiting(Pid, State = #session{response_pid    = undefined,
                                   session_timeout = Ref}) ->
    link(Pid),
    _ = case Ref of
            undefined -> ok;
            _         -> erlang:cancel_timer(Ref)
        end,
    State#session{response_pid    = Pid,
                  session_timeout = undefined};
mark_waiting(Pid, State = #session{response_pid    = Pid,
                                   session_timeout = undefined}) ->
    %% The same process may ask for messages multiple times.
    State.

unmark_waiting(State = #session{response_pid = RPid,
                                session_timeout = undefined,
                                disconnect_delay = DisconnectDelay}) ->
    case RPid of
        undefined -> ok;
        _Else ->
            unlink(RPid)
    end,
    Ref = erlang:send_after(DisconnectDelay, self(), session_timeout),
    State#session{response_pid    = undefined,
                  session_timeout = Ref};
unmark_waiting(State = #session{response_pid = undefined,
                                session_timeout = Ref,
                                disconnect_delay = DisconnectDelay}) ->
    _ = erlang:cancel_timer(Ref),
    Ref1 = erlang:send_after(DisconnectDelay, self(), session_timeout),
    State#session{session_timeout = Ref1}.

emit(What, #session{callback = Callback,
                    handle = Handle}) ->
    Callback(Handle, What).

%% --------------------------------------------------------------------------

-spec init({session_or_undefined(), service()}) -> {ok, #session{}}.
init({SessionId, #service{callback         = Callback,
                          disconnect_delay = DisconnectDelay}}) ->
    case SessionId of
        undefined -> ok;
        _Else     -> ets:insert(?ETS, {SessionId, self()})
    end,
    process_flag(trap_exit, true),
    {ok, #session{id = SessionId,
                  callback = Callback,
                  disconnect_delay = DisconnectDelay,
                  handle = {?MODULE, {sockjs_util:guid(), self()}}}}.


handle_call({reply, _Pid, _Multiple}, _From, State = #session{
                                               ready_state = connecting}) ->
    emit(init, State),
    State1 = unmark_waiting(State),
    {reply, {ok, {open, nil}},
     State1#session{ready_state = open}};

handle_call({reply, _Pid, _Multiple}, _From, State = #session{
                                               ready_state = closed,
                                               close_msg = CloseMsg}) ->
    State1 = unmark_waiting(State),
    {reply, {close, {close, CloseMsg}}, State1};

handle_call({reply, Pid, Multiple}, _From, State = #session{
                                             ready_state = open,
                                             response_pid = RPid,
                                             outbound_queue = Q}) ->
    {Messages, Q1} = case Multiple of
                         true  -> {queue:to_list(Q), queue:new()};
                         false -> case queue:out(Q) of
                                      {{value, Msg}, Q2} -> {[Msg], Q2};
                                      {empty, Q2}        -> {[], Q2}
                                  end
                     end,
    case {Messages, RPid} of
        {[], P} when P =:= undefined orelse P =:= Pid ->
            State1 = mark_waiting(Pid, State),
            {reply, wait, State1};
        {[], _} ->
            %% don't use reply(), this shouldn't touch the session lifetime
            {reply, session_in_use, State};
        {_, _} ->
            State1 = unmark_waiting(State),
            {reply, {ok, {data, Messages}},
             State1#session{outbound_queue = Q1}}
    end;

handle_call({received, Messages}, _From, State = #session{ready_state = open}) ->
    _ = [ emit({recv, iolist_to_binary(Msg)}, State) ||
            Msg <- Messages],
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
            State = #session{response_pid = Pid}) ->
    %% It is illegal for a connection to go away when receiving, we
    %% may lose some messages that are in transit. Kill current
    %% session.
    {stop, normal, State#session{response_pid = undefined}};

handle_info(session_timeout, State = #session{response_pid = undefined}) ->
    {stop, normal, State};

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.


terminate(normal, State = #session{id = SessionId}) ->
    ets:delete(?ETS, SessionId),
    emit(closed, State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

