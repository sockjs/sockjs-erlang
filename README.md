SockJS family:

  * [SockJS-client](https://github.com/sockjs/sockjs-client) JavaScript client library
  * [SockJS-node](https://github.com/sockjs/sockjs-node) Node.js server
  * [SockJS-erlang](https://github.com/sockjs/sockjs-erlang) Erlang server


SockJS-erlang server
====================

[SockJS](http://sockjs.org) server written in Erlang. Can run with
[Cowboy](https://github.com/extend/cowboy) or
[Misultin](https://github.com/ostinelli/misultin). SockJS-erlang is
compatible with
[SockJS client version 0.2](http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.2.html). See
https://github.com/sockjs/sockjs-client for more information on SockJS.


Show me the code!
-----------------

### Cowboy

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

### Misultin

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
            sockjs_misultin_handler:handle_ws(SockjsState, Req);
        false ->
            closed
    end.

service_echo(Conn, {recv, Data}) -> sockjs:send(Data, Conn);
service_echo(_Conn, _)           -> ok.
```

Dig into the `examples` directory to get working code:

  * https://github.com/sockjs/sockjs-erlang/examples/cowboy_echo.erl
  * https://github.com/sockjs/sockjs-erlang/examples/misultin_echo.erl

How to run the examples?
------------------------

You may need a recent version of Erlang/OTP, at least R14B is recommended.

To run Cowboy example:

    cd sockjs-erlang
    ./rebar -C rebar-cowboy.config get-deps
    ./rebar -C rebar-cowboy.config compile
    ./examples/cowboy_echo.erl

To run Misultin example:

    cd sockjs-erlang
    ./rebar -C rebar-misultin.config get-deps
    ./rebar -C rebar-misultin.config compile
    ./examples/misultin_echo.erl

This will start a simple `/echo` SockJS server on
`http://localhost:8081`.  Open this link in a browser and play
around.


SockJS-erlang API
-----------------

Except for the web framework-specific API's, SockJS-erlang is rather
simple. It has just a couple of methods:

 * **sockjs_handler:init_state(prefix, callback, options) -> service()**

    Initializes the state of a SockJS service (ie: a thing you can
    access from the browser, it has an url and a code on the server
    side). `prefix` is a binary that must exacty match the url prefix
    of the service, for example, if service will be listening on
    '/echo', this parameter must be set to `<<"/echo">>`. `callback`
    function will be called when a new SockJS connection is
    established, data received or a connection is closed. Options is a
    proplist that can contain following tuples:

     * `{sockjs_url, string()}` - Transports which don't support
       cross-domain communication natively ('eventsource' to name one)
       use an iframe trick.  A simple page is served from the SockJS
       server (using its foreign domain) and is placed in an invisible
       iframe. Code run from this iframe doesn't need to worry about
       cross-domain issues, as it's being run from domain local to the
       SockJS server. This iframe also does need to load SockJS
       javascript client library, and this option lets you specify its
       url (if you're unsure, point it to <a
       href="http://cdn.sockjs.org/sockjs-0.2.min.js"> the latest
       minified SockJS client release</a>, this is the default).
     * `{websocket, boolean()}` - are native websockets enabled? This
       can be usefull when your loadbalancer doesn't support them.
     * `{cookie_needed, boolean()}` - is your load balancer relying on
       cookies to get sticky sessions working?
     * `{heartbeat_delay, integer()}` - how often to send heartbeat
       packets (in ms).
     * `{disconnect_delay, integer()}` - how long to hold session state
       after the client was last connected (in ms).
     * `{response_limit, integer()}` - the maximum size of a single
       http streaming response (in bytes).
     * `{logger, fun/3}` - a function called on every request, used
       to print request to the logs (or on the screen by default).

    For more explanation, please do take a look at
    [SockJS-node readme](https://github.com/sockjs/sockjs-node/blob/master/README.md).

 * **sockjs:send(connection, payload) -> ok**

     Send data over an active SockJS connection. Payload should be of
     iodata() type. Messages sent after connection gets closed will be
     lost.

 * **sockjs:close(connection, code, reason) -> ok**

     Close an active SockJS connection with code and reason. If code
     and reason are skipped, the defaults are used.


The framework-specific calls are more problematic. Instead of trying
to explain how to use them, please take a look at the examples.

 * **type(req() :: {cowboy|misultin, request()})**
 * **sockjs_handler:handle_req(service(), req()) -> req()**
 * **sockjs_handler:handle_ws(service(), req()) -> req()**

Stability
---------

SockJS-erlang is quite new, but should be reasonably stable.

Cowboy is passing almost all the
[SockJS-protocol tests](https://github.com/sockjs/sockjs-protocol). The
one exception is described in this issue:

 * https://github.com/extend/cowboy/issues/140

Misultin is behaving well most of the time, with the exception of a
few (mostly websocket related) issues:

 * https://github.com/ostinelli/misultin/issues/98
 * https://github.com/ostinelli/misultin/issues/99
 * https://github.com/ostinelli/misultin/issues/101
 * https://github.com/ostinelli/misultin/issues/102

Deployment and load balancing
-----------------------------

SockJS servers should work well behind many load balancer setups, but
it sometimes requres some additional twaks.  For more details, please
do take a look at the 'Deployment' section in
[SockJS-node readme](https://github.com/sockjs/sockjs-node/blob/master/README.md).

Development and testing
-----------------------

You need [rebar](https://github.com/basho/rebar)
([instructions](https://github.com/basho/rebar/wiki/Building-rebar)).
Due to a bug in rebar config handling you need a reasonably recent
version - newer than late Oct 2011. Alternatively, SockJS-erlang is
bundeled with a recent rebar binary.

SockJS-erlang contains a `test_server` for both Cowboy and Misultin,
which is a simple server used for testing.

To run Cowboy test_server:

    cd sockjs-erlang
    ./rebar -C rebar-cowboy.config get-deps
    ./rebar -C rebar-cowboy.config compile
    ./examples/cowboy_test_server.erl

To run Misultin test_server:

    cd sockjs-erlang
    ./rebar -C rebar-misultin.config get-deps
    ./rebar -C rebar-misultin.config compile
    ./examples/misultin_test_server.erl

That should start test_server on port 8081. Currently, there are two
separate test suits using test_server.

### SockJS-protocol Python tests

Once test_server is listening on `http://localhost:8081` you may test it
using SockJS-protocol:

    cd sockjs-protocol
    make test_deps
    ./venv/bin/python sockjs-protocol-dev.py

For details see
[SockJS-protocol README](https://github.com/sockjs/sockjs-protocol#readme).

### SockJS-client QUnit tests

You need to start a second web server (by default listening on 8080)
that is serving various static html and javascript files:

    cd sockjs-client
    make test

At that point you should have two web servers running: sockjs-erlang on
8081 and sockjs-client on 8080. When you open the browser on
[http://localhost:8080/](http://localhost:8080/) you should be able
run the QUnit tests against your sockjs-node server.

For details see
[SockJS-client README](https://github.com/sockjs/sockjs-client#readme).

Additionally, if you're doing more serious development consider using
`make serve`, which will automatically the server when you modify the
source code.
