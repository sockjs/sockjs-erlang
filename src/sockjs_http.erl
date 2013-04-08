-module(sockjs_http).

-export([path/1, method/1, body/1, body_qs/1, header/2, jsessionid/1,
         callback/1, peername/1, sockname/1]).
-export([reply/4, chunk_start/3, chunk/2, chunk_end/1]).
-export([hook_tcp_close/1, unhook_tcp_close/1, abruptly_kill/1]).
-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec path(req()) -> {string(), req()}.
path({cowboy, Req})       -> {Path, Req1} = cowboy_req:path(Req),
                             {binary_to_list(Path), {cowboy, Req1}}.

-spec method(req()) -> {atom(), req()}.
method({cowboy, Req})       -> {Method, Req1} = cowboy_req:method(Req),
                               {method_atom(Method), {cowboy, Req1}}.

-spec method_atom(binary() | atom()) -> atom().
method_atom(<<"GET">>) -> 'GET';
method_atom(<<"PUT">>) -> 'PUT';
method_atom(<<"POST">>) -> 'POST';
method_atom(<<"DELETE">>) -> 'DELETE';
method_atom(<<"OPTIONS">>) -> 'OPTIONS';
method_atom(<<"PATCH">>) -> 'PATCH';
method_atom(<<"HEAD">>) -> 'HEAD';
method_atom('GET') -> 'GET';
method_atom('PUT') -> 'PUT';
method_atom('POST') -> 'POST';
method_atom('DELETE') -> 'DELETE';
method_atom('OPTIONS') -> 'OPTIONS';
method_atom('PATCH') -> 'PATCH';
method_atom('HEAD') -> 'HEAD'.

-spec body(req()) -> {binary(), req()}.
body({cowboy, Req})       -> {ok, Body, Req1} = cowboy_req:body(Req),
                             {Body, {cowboy, Req1}}.

-spec body_qs(req()) -> {binary(), req()}.
body_qs(Req) ->
    {H, Req1} =  header('content-type', Req),
    case H of
        H when H =:= "text/plain" orelse H =:= "" ->
            body(Req1);
        _ ->
            %% By default assume application/x-www-form-urlencoded
            body_qs2(Req1)
    end.
body_qs2({cowboy, Req}) ->
    {ok, BodyQS, Req1} = cowboy_req:body_qs(Req),
    case proplists:get_value(<<"d">>, BodyQS) of
        undefined ->
            {<<>>, {cowboy, Req1}};
        V ->
            {V, {cowboy, Req1}}
    end.

-spec header(atom(), req()) -> {nonempty_string() | undefined, req()}.
header(K, {cowboy, Req})->
    {H, Req2} = cowboy_req:header(K, Req),
    {V, Req3} = case H of
                    undefined ->
                        cowboy_req:header(atom_to_binary(K, utf8), Req2);
                    _ -> {H, Req2}
                end,
    case V of
        undefined -> {undefined, {cowboy, Req3}};
        _         -> {binary_to_list(V), {cowboy, Req3}}
    end.

-spec jsessionid(req()) -> {nonempty_string() | undefined, req()}.
jsessionid({cowboy, Req}) ->
    {C, Req2} = cowboy_req:cookie(<<"jsessionid">>, Req),
    case C of
        _ when is_binary(C) ->
            {binary_to_list(C), {cowboy, Req2}};
        undefined ->
            {undefined, {cowboy, Req2}}
    end.

-spec callback(req()) -> {nonempty_string() | undefined, req()}.
callback({cowboy, Req}) ->
    {CB, Req1} = cowboy_req:qs_val(<<"c">>, Req),
    case CB of
        undefined -> {undefined, {cowboy, Req1}};
        _         -> {binary_to_list(CB), {cowboy, Req1}}
    end.

-spec peername(req()) -> {{inet:ip_address(), non_neg_integer()}, req()}.
peername({cowboy, Req}) ->
    {P, Req1} = cowboy_req:peer(Req),
    {P, {cowboy, Req1}}.

-spec sockname(req()) -> {{inet:ip_address(), non_neg_integer()}, req()}.
sockname({cowboy, Req} = R) ->
    {Addr, _Req} = cowboy_req:peer(Req),
    {Addr, R}.

%% --------------------------------------------------------------------------

-spec reply(non_neg_integer(), headers(), iodata(), req()) -> req().
reply(Code, Headers, Body, {cowboy, Req}) ->
    Body1 = iolist_to_binary(Body),
    {ok, Req1} = cowboy_req:reply(Code, enbinary(Headers), Body1, Req),
    {cowboy, Req1}.

-spec chunk_start(non_neg_integer(), headers(), req()) -> req().
chunk_start(Code, Headers, {cowboy, Req}) ->
    {ok, Req1} = cowboy_req:chunked_reply(Code, enbinary(Headers), Req),
    {cowboy, Req1}.

-spec chunk(iodata(), req()) -> {ok | error, req()}.
chunk(Chunk, {cowboy, Req} = R) ->
    case cowboy_req:chunk(Chunk, Req) of
        ok          -> {ok, R};
        {error, _E} -> {error, R}
                      %% This shouldn't happen too often, usually we
                      %% should catch tco socket closure before.
    end.

-spec chunk_end(req()) -> req().
chunk_end({cowboy, _Req} = R)  -> R.

enbinary(L) -> [{list_to_binary(K), list_to_binary(V)} || {K, V} <- L].


-spec hook_tcp_close(req()) -> req().
hook_tcp_close(R = {cowboy, Req}) ->
    [T, S] = cowboy_req:get([transport, socket], Req),
    T:setopts(S,[{active,once}]),
    R.

-spec unhook_tcp_close(req()) -> req().
unhook_tcp_close(R = {cowboy, Req}) ->
    [T, S] = cowboy_req:get([transport, socket], Req),
    T:setopts(S,[{active,false}]),
    R.

-spec abruptly_kill(req()) -> req().
abruptly_kill(R = {cowboy, Req}) ->
    [T, S] = cowboy_req:get([transport, socket], Req),
    ok = T:close(S),
    R.
