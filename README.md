SockJS family:

  * [SockJS-client](https://github.com/sockjs/sockjs-client) JavaScript client library
  * [SockJS-node](https://github.com/sockjs/sockjs-node) Node.js server
  * [SockJS-erlang](https://github.com/sockjs/sockjs-erlang) Erlang server


SockJS-erlang
=============

Erlang-based server side component for
[SockJS](http://sockjs.org). Can run on Cowboy or Misultin.

See https://github.com/sockjs/sockjs-client for more information on SockJS.

Currently this is somewhat limited:

* A couple of header-based things haven't been ported from the node server
* Hasn't been tested much
* Hasn't been performance-tested at all

Hopefully in future some of these restrictions will get improved.

See the `sockjs_test` module for what passes for usage examples.

Some useful application configuration parameters:

* **sockjs_url** - path to the sockjs library as seen by the client
 * default: "/lib/sockjs.js"
* **heartbeat_ms** - milliseconds between heartbeat frames
 * default: 25000
* **session_close_ms** - milliseconds to keep a session alive after a client connection goes away
 * default: 5000
* **json_impl** - JSON library to use, mochijson2 or eep0018
 * default: mochijson2

[Note that the tests test that you can send unicode strings containing \0.
When using eep0018 you can't; it strips them out. So a few tests will fail.]


Development
-----------

To run tests you need to have `node` and `coffee-script` installed

    cd sockjs-erlang
    npm install coffee-script

Now you're ready to run test server, if you want to use `cowboy`:

    make test

Or `misultin`:

    make test HTTP=misultin

