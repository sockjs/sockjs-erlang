-module(sockjs_filters).

-export([filters/1]).
-export([xhr_polling/4, xhr_streaming/4, xhr_send/4, jsonp/4, jsonp_send/4]).

filters(Endpoint) ->
    case proplists:get_value(list_to_atom(Endpoint), filters()) of
        undefined -> exit({unknown_endpoint, Endpoint});
        Filters   -> Filters
    end.

filters() ->
    %% websocket does not actually go via transport:handle_req/3 but we need
    %% something in transport:dispatch/2
    [{websocket,     []},
     {xhr_send,      [xhr_send]},
     {xhr,           [xhr_polling]},
     {xhr_streaming, [xhr_streaming]},
     {jsonp_send,    [jsonp_send]},
     {jsonp,         [jsonp]}].

%% --------------------------------------------------------------------------

%% This is send but it receives - "send" from the client POV, receive
%% from ours.
xhr_send(Req, _Server, SessionId, Receive) ->
    receive_body(Req:get(body), SessionId, Receive),
    Req:respond(204).

xhr_polling(Req, _Server, SessionId, _Receive) ->
    headers(Req),
    reply_loop(Req, SessionId, true, fun fmt_xhr/1).

%% TODO Do something sensible with client closing timeouts
xhr_streaming(Req, _Server, SessionId, _Receive) ->
    headers(Req),
    %% IE requires 2KB prefix:
    %% http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
    chunk(Req, list_to_binary(string:copies("h", 2048)), fun fmt_xhr/1),
    reply_loop(Req, SessionId, false, fun fmt_xhr/1).

jsonp_send(Req, _Server, SessionId, Receive) ->
    Body = proplists:get_value("d", Req:parse_post()),
    receive_body(Body, SessionId, Receive),
    Req:respond(200, "").

jsonp(Req, _Server, SessionId, _Receive) ->
    headers(Req),
    CB = list_to_binary(proplists:get_value("c", Req:parse_qs())),
    reply_loop(Req, SessionId, true, fun (Body) -> fmt_jsonp(Body, CB) end).

%% --------------------------------------------------------------------------

receive_body(Body, SessionId, Receive) ->
    Decoded = mochijson2:decode(Body),
    Sender = sockjs_session:sender(SessionId),
    [Receive(Sender, {recv, Msg}) || Msg <- Decoded].

headers(Req) ->
    headers(Req, "application/javascript; charset=UTF-8").

headers(Req, ContentType) ->
    Req:chunk(head, [{"Content-Type", ContentType}]).

reply_loop(Req, SessionId, Once, Fmt) ->
    case sockjs_session:reply(SessionId) of
        wait  -> receive
                     go -> reply_loop(Req, SessionId, Once, Fmt)
                 after 5000 ->
                         chunk(Req, <<"h">>, Fmt),
                         reply_loop0(Req, SessionId, Once, Fmt)
                 end;
        Reply -> chunk(Req, Reply, Fmt),
                 reply_loop0(Req, SessionId, Once, Fmt)
    end.

reply_loop0(Req, _SessionId, true, _Fmt) ->
    io:format("end!~n"),
    Req:chunk(done);
reply_loop0(Req, SessionId, false, Fmt) ->
    reply_loop(Req, SessionId, false, Fmt).

chunk(Req, Body, Fmt) ->
    Req:chunk(Fmt(Body)).

fmt_xhr(Body) -> <<Body/binary, $\n>>.

fmt_jsonp(Body, Callback) ->
    %% Yes, JSONed twice, there isn't a a better way, we must pass
    %% a string back, and the script, will be evaled() by the
    %% browser.
    Double = iolist_to_binary(mochijson2:encode(Body)),
    <<Callback/binary, "(", Double/binary, ");", $\r, $\n>>.
