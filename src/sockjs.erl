%% Public interface. In general, use sockjs_cowboy or sockjs_misultin
%% to start a listener, and from then on use sockjs:*.

-module(sockjs).

-export([respond/4, path/1, req/1]).
-export([send/2, close/3]).

%% Get the path of a request. A convenience; use sockjs:req to get the
%% underlying request, which will be specific to the web server
%% implementation.
path(Req) ->
    sockjs_http:path(Req).

%% Get the underlying request; this is specific to the web server. Use
%% the convenience functions in this module instead to remain web
%% server neutral.
req({cowboy, Req})   -> Req;
req({misultin, Req}) -> Req.

%% Respond to a request (e.g., in a fallback request handler).
respond(Code, Headers, Content, Req) ->
    sockjs_http:reply(Code, Headers, Content, Req).


%% Send data over a connection.
send(Data, Self = {sockjs_ws, _Conn, _Http}) ->
    sockjs_ws:send(Data, Self);
send(Data, Self = {sockjs_session, _SessionId}) ->
    sockjs_session:send(Data, Self).

%% Initiate a close of a connection.
close(Code, Reason, Self = {sockjs_session, _SessionId}) ->
    sockjs_session:close(Code, Reason, Self);
close(Code, Reason, Self = {sockjs_ws, _Conn, _Http}) ->
    sockjs_ws:close(Code, Reason, Self).
