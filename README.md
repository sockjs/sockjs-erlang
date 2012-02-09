SockJS family:

  * [SockJS-client](https://github.com/sockjs/sockjs-client) JavaScript client library
  * [SockJS-node](https://github.com/sockjs/sockjs-node) Node.js server
  * [SockJS-erlang](https://github.com/sockjs/sockjs-erlang) Erlang server


SockJS-erlang server
====================

[SockJS](http://sockjs.org) server written in Erlang. Can run with
[Cowboy](https://github.com/extend/cowboy) or
[Misultin](https://github.com/ostinelli/misultin).

See https://github.com/sockjs/sockjs-client for more information on
SockJS.

A simplistic echo SockJS server using Cowboy may look more or less
like this:

```erlang
main(_) ->
    application:start(sockjs),
    application:start(cowboy),

    SockjsState = sockjs_handler:init_state(
                    <<"/echo">>, fun service_echo/2, []),

    Routes = [{'_',  [{[<<"echo">>, '...'],
                       sockjs_cowboy_handler, SockjsState}]}],

    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port,     8081}],
                          cowboy_http_protocol, [{dispatch, Routes}]),
    receive
        _ -> ok
    end.

service_echo(Conn, {recv, Data}) -> sockjs:send(Data, Conn);
service_echo(_Conn, _)           -> ok.
```

And a simplistic echo SockJS server using Misultin may look more or
less like this:

```erlang
main(_) ->
    application:start(sockjs),

    SockjsState = sockjs_handler:init_state(
                    <<"/echo">>, fun service_echo/2, []),

    misultin:start_link(
      [{port, 8081}, {autoexit, false}, {ws_autoexit, false},
       {loop,    fun (Req) -> handle_http(Req, SockjsState) end},
       {ws_loop, fun (Req) -> handle_ws(Req, SockjsState) end}]),
    receive
        _ -> ok
    end.

handle_http(Req, SockjsState) ->
    case Req:resource([]) of
        ["echo" | _T] ->
            sockjs_handler:handle_req(SockjsState, {misultin, Req});
        _Any ->
            Req:respond(404,
                        <<"404 - Nothing here (via sockjs-erlang fallback)\n">>)
    end.

handle_ws(Req, SockjsState) ->
    case string:tokens(Req:get(path), "/") of
        ["echo" | _T] ->
            sockjs_misultin_handler:handle_ws(SockjsState, {misultin, Req});
        false ->
            closed
    end.

service_echo(Conn, {recv, Data}) -> sockjs:send(Data, Conn);
service_echo(_Conn, _)           -> ok.
```

Dig into the `examples` directory for more details:

  * https://github.com/sockjs/sockjs-erlang/examples/simple_cowboy.erl
  * https://github.com/sockjs/sockjs-erlang/examples/simple_misultin.erl

SockJS-erlang API
-----------------

Except for the web framework-specific API's, SockJS-erlang is rather
simple. It has just a couple of methods:

 * **sockjs_handler:init_state(prefix, callback, options) -> service()**
 * **sockjs:send(payload) -> ok**
 * **sockjs:close(code, reason) -> ok**

The framework-specific calls are more problematic. Instead of trying
to explain how to use them, please take a look at the examples.

 ** req() :: {cowboy|misultin, request} **

 * **sockjs_handler:handle_req(service(), req()) -> req()**
 * **sockjs_handler:handle_ws(service(), req()) -> req()**

Status
------

SockJS-erlang is quite new, but should be reasonably stable.

Cowboy is passing all the tests.

Misulting is behaving well most of the time, with the exception of a
few websockets issues:

 * https://github.com/ostinelli/misultin/issues/98
 * https://github.com/ostinelli/misultin/issues/99

Development
-----------

To run Cowboy examples:

    cd sockjs-erlang
    rebar -C rebar-cowboy.config get-deps
    rebar -C rebar-cowboy.config compile
    ./examples/simple_cowboy.erl


To run Misultin examples:

    cd sockjs-erlang
    rebar -C rebar-misultin.config get-deps
    rebar -C rebar-misultin.config compile
    ./examples/simple_misultin.erl

This should start a simple `/echo` server on `http://localhost:8081`.
