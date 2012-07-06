%% ***** BEGIN LICENSE BLOCK *****
%% Copyright (c) 2011-2012 VMware, Inc.
%%
%% For the license see COPYING.
%% ***** END LICENSE BLOCK *****

-type(req()          :: {cowboy, any()}).

-type(user_session() :: nonempty_string()).
-type(emittable()    :: init|closed|{recv, binary()}|{info, any()}).
-type(callback()     :: fun((user_session(), emittable(), any()) -> ok)).
-type(logger()       :: fun((any(), req(), websocket|http) -> req())).

-record(service, {prefix           :: nonempty_string(),
                  callback         :: callback(),
                  state            :: any(),
                  sockjs_url       :: nonempty_string(),
                  cookie_needed    :: boolean(),
                  websocket        :: boolean(),
                  disconnect_delay :: non_neg_integer(),
                  heartbeat_delay  :: non_neg_integer(),
                  response_limit   :: non_neg_integer(),
                  hib_timeout      :: non_neg_integer() | hibernate,
                  logger           :: logger()
                  }).

-type(service() :: #service{}).

-type(headers() :: list({nonempty_string(), nonempty_string()})).
-type(server()  :: nonempty_string()).
-type(session() :: nonempty_string()).

-type(frame()   :: {open, nil} | {close, {non_neg_integer(), string()}} | {data, list(iodata())} | {heartbeat, nil} ).

-type(info()    :: [{atom(), any()}]).
