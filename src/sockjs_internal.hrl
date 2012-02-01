
-record(state, {prefix :: nonempty_string(),
                service_callback,
                url :: nonempty_string(),
                cookie_needed :: boolean(),
                websocket :: boolean()
                       }).

-type(state() :: #state{}).

-type(req() :: {cowboy, any()} | {misultin, any()}).
-type(headers() :: list({nonempty_string(), nonempty_string()})).
