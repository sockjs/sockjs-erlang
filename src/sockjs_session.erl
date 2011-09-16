-module(sockjs_session).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-include("sockjs.hrl").

-export([start_link/1, maybe_create/3, with/2, with_sync/2]).

start_link(Receiver) ->
    gen_server:start_link(?MODULE, Receiver, []).

maybe_create(Module, SessionId, Fun) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> Receiver = {Module, SessionId},
                       {ok, SPid} = sockjs_session_sup:start_child(Receiver),
                       ets:insert(?ETS, {SessionId, SPid}),
                       Receiver:open(),
                       Fun(Receiver, init),
                       SPid;
        [{_, SPid}] -> SPid
    end.

with(Fun, SessionId) ->
    with0(fun (SPid) -> gen_server:cast(SPid, {with, Fun}) end, SessionId).

with_sync(Fun, SessionId) ->
    with0(fun (SPid) -> gen_server:call(SPid, {with, Fun}, infinity) end,
          SessionId).

with0(Fun, SessionId) ->
    case ets:lookup(?ETS, SessionId) of
        []          -> exit(no_session);
        [{_, SPid}] -> Fun(SPid)
    end.

%% --------------------------------------------------------------------------

init(Receiver = {_Module, SessionId}) ->
    {ok, #session{id       = SessionId,
                  receiver = Receiver}}.

handle_call({with, Fun}, _From, State) ->
    {Reply, State2} = Fun(State),
    {reply, Reply, State2};

handle_call(Request, _From, State) ->
    {stop, {odd_request, Request}, State}.

handle_cast({with, Fun}, State) ->
    {noreply, Fun(State)};

handle_cast(Cast, State) ->
    {stop, {odd_cast, Cast}, State}.

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

