Erlang-based server side component for SockJS. See:

https://github.com/majek/sockjs-client

for more information on SockJS.

Currently this is somewhat limited:

* Requires Misultin
* ...and yet embeds mochijson2.
* Supports WebSockets via Misultin (so no hybi-10!)
* A couple of header-based things haven't been ported from the node server
* Sessions never time out
* Hasn't been tested much
* Hasn't been performance-tested at all

Hopefully in future some of these restrictions will get improved.
