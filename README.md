SockJS family:

  * [SockJS-client](https://github.com/sockjs/sockjs-client) JavaScript client library
  * [SockJS-node](https://github.com/sockjs/sockjs-node) Node.js server
  * [SockJS-erlang](https://github.com/sockjs/sockjs-erlang) Erlang server

SockJS-erlang
=============

Erlang-based server side component for
[SockJS](http://sockjs.org). Can run with Cowboy or Misultin.

See https://github.com/sockjs/sockjs-client for more information on SockJS.

Status
------

Currently this lags the Node.JS implementation slightly:

* A couple of header-based things haven't been ported from the node server
* Hasn't been tested much
* Hasn't been performance-tested at all

Some useful application configuration parameters:

* **sockjs_url** - path to the sockjs library as seen by the client
 (default: "/lib/sockjs.js")
* **heartbeat_ms** - milliseconds between heartbeat frames
 (default: 25000)
* **session_close_ms** - milliseconds to keep a session alive after a client connection goes away
 (default: 5000)
* **json_impl** - JSON library to use, either jiffy, mochijson2 or eep0018
 (default: jiffy (NIF-based))

[Note that the tests test that you can send unicode strings containing \0.
When using eep0018 you can't; it strips them out. So a few tests will fail.]

Using SockJS-erlang
-------------------

You will need either [cowboy](https://github.com/extend/cowboy) or
[misultin](https://github.com/ostinelli/misultin) installed.

### Using cowboy

The module `sockjs_cowboy` implements both the cowboy_http_handler and
cowboy_http_websocket_handler behaviours. The procedure
`sockjs_cowboy:init_state/2` gives you a initial state for the
handler, given a fallback handler and a dispatch table (both explained
in a minute, after we visit misultin too).

    application:start(sockjs),
    application:start(cowboy),
    Routes = [{'_', [{'_', sockjs_cowboy,
                           sockjs_cowboy:init_state(Fallback, Dispatch)}]}],
             cowboy:start_listener(http, 100,
                                   cowboy_tcp_transport, [{port,     Port}],
                                   cowboy_http_protocol, [{dispatch, Routes}])

### Using misultin

The procedure `sockjs_misultin:init_state/2` produces a request loop
and a websocket accept loop suitable for use with misultin, given a
fallback handler and a dispatch table.

    application:start(sockjs),
    {Loop, WsLoop} = sockjs_misultin:init_state(Fallback, Dispatch),
    {ok, _} = misultin:start_link([{loop,        Loop},
                                   {ws_loop,     WsLoop},
                                   {ws_autoexit, false},
                                   {port,        Port}]);

### Fallback handler and dispatch table

The arguments to `sockjs_cowboy:init_state` and
sockjs_misultin:init_state` are a procedure for handling requests that
are not handled by SockJS, and a table of SockJS endpoints.

The fallback is supplied a wrapped request; the procedures in
`sockjs` may be used to retrieve information in a webserver-neutral
way, or the request unwrapped with `sockjs:req/1`.

    fallback(Request) ->
        "/" ++ Path = sockjs:path(Request),
        case Path of
            "index.html" ->
                sockjs:respond(200, [{"Content-Type", "text/html"}],
                               "<html> ... </html>", Request)
            _ ->
                sockjs:respond(404, [], "Not found")
        end
    end.

The dispatch table gives a map of URL prefixes to SockJS handlers:

    [{echo,      fun echo/2},
     {broadcast, fun broadcast/2}]

This table says that SockJS connections to `/echo` shall be handled by
`echo/2` and connections to `/broadcast` shall be handled by
`broadcast/2`. Those two procedures are supplied the connection, and a
term representing the connection event (initiation, message received,
etc.).

Development
-----------

SockJS has a rebar config and may be included as a dependency of rebar
projects. SockJS does not itself depend on a web server.

To compile from the source repo:

    $ make

To run tests you need to have `node` and `coffee-script` installed

    $ npm install coffee-script

Now you're ready to run test server. If you want to use `cowboy`:

    $ make test

Or `misultin`:

    $ make test HTTP=misultin

Both of these will clone the appropriate web server source from github
and build it. Run the tests or see a demo by visiting
`http://localhost:8080`.
