Erlang-based server side component for SockJS. See:

https://github.com/sockjs/sockjs-client

for more information on SockJS.

Currently this is somewhat limited:

* Requires Misultin
* Supports WebSockets via Misultin (so no hybi-10!)
* A couple of header-based things haven't been ported from the node server
* Hasn't been tested much
* Hasn't been performance-tested at all

Hopefully in future some of these restrictions will get improved.

Some useful application configuration parameters:

sockjs_url:       path to the sockjs library as seen by the client
                  default: "/lib/sockjs.js"

heartbeat_ms:     milliseconds between heartbeat frames
                  default: 25000

session_close_ms: milliseconds to keep a session alive after a client
                  connection goes away
                  default: 5000

json_impl:        JSON library to use, mochijson2 or eep0018
                  default: mochijson2
