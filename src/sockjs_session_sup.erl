-module(sockjs_session_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

start_link() ->
     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

start_child(SessionId, Receive) ->
    supervisor:start_child(
      ?MODULE,
      {SessionId, {sockjs_session, start_link, [SessionId, Receive]},
       transient, 16#ffffffff, worker, [sockjs_session]}).
