
-record(service, {prefix :: nonempty_string(),
                  callback :: callback(),
                  sockjs_url :: nonempty_string(),
                  cookie_needed :: boolean(),
                  websocket :: boolean(),
                  disconnect_delay :: non_neg_integer(),
                  heartbeat_delay :: non_neg_integer(),
                  response_limit :: non_neg_integer(),
                  logger :: fun((service(), req(), websocket|http) -> req())
                        }).

-type(service() :: #service{}).

-type(req() :: {cowboy, any()} | {misultin, any()}).
-type(headers() :: list({nonempty_string(), nonempty_string()})).
-type(server() :: nonempty_string()).
-type(session() :: nonempty_string()).
-type(user_session() :: nonempty_string()).
-type(callback() :: fun((user_session(), init|closed|{recv, binary()}) -> ok)).

-type(frame() :: {open, nil} |
                 {close, {non_neg_integer(), string()}} |
                 {data, list(iodata())} |
                 {heartbeat, nil} ).
