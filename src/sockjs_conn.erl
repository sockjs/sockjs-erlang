-module(sockjs_conn).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {send, 2},
     {close, 3}
    ];
behaviour_info(_Other) ->
    undefined.
