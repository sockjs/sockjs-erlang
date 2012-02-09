-module(sockjs_http).

-export([path/1, method/1, body/1, body_qs/1, header/2, jsessionid/1,
         callback/1]).
-export([reply/4, chunk_start/3, chunk/2, chunk_end/1]).
-export([hook_tcp_close/1, unhook_tcp_close/1, abruptly_kill/1]).
-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec path(req()) -> {string(), req()}.
path({cowboy, Req})       -> {Path, Req1} = cowboy_http_req:raw_path(Req),
                             {binary_to_list(Path), {cowboy, Req1}};
path({misultin, Req} = R) -> case element(1, Req) of
                                 misultin_ws -> {Req:get(path), R};
                                 _           -> {abs_path, Path} = Req:get(uri),
                                                {Path, R}
                             end.

-spec method(req()) -> {atom(), req()}.
method({cowboy, Req})       -> {Method, Req1} = cowboy_http_req:method(Req),
                               {Method, {cowboy, Req1}};
method({misultin, Req} = R) -> case element(1, Req) of
                                   misultin_ws -> {'GET', R};
                                   _           -> {Req:get(method), R}
                               end.

-spec body(req()) -> {binary(), req()}.
body({cowboy, Req})       -> {ok, Body, Req1} = cowboy_http_req:body(Req),
                             {Body, {cowboy, Req1}};
body({misultin, Req} = R) -> {Req:get(body), R}.

-spec body_qs(req()) -> {binary(), req()}.
body_qs(Req) ->
    {H, Req1} =  header('Content-Type', Req),
    case H of
        H when H =:= "text/plain" orelse H =:= "" ->
            body(Req1);
        _ ->
            %% By default assume application/x-www-form-urlencoded
            body_qs2(Req1)
    end.
body_qs2({cowboy, Req}) ->
    {BodyQS, Req1} = cowboy_http_req:body_qs(Req),
    case proplists:get_value(<<"d">>, BodyQS) of
        undefined ->
            {<<>>, {cowboy, Req1}};
        V ->
            {V, {cowboy, Req1}}
    end;
body_qs2({misultin, Req} = R) ->
    case proplists:get_value("d", Req:parse_post()) of
        undefined -> {<<>>, R};
        V         -> {iolist_to_binary(V), R}
    end.

-spec header(atom(), req()) -> {nonempty_string() | undefined, req()}.
header(K, {cowboy, Req})->
    {H, Req2} = cowboy_http_req:header(K, Req),
    {V, Req3} = case H of
                    undefined ->
                        cowboy_http_req:header(atom_to_binary(K, utf8), Req2);
                    _ -> {H, Req2}
                end,
    case V of
        undefined -> {undefined, {cowboy, Req3}};
        _         -> {binary_to_list(V), {cowboy, Req3}}
    end;

header(K, {misultin, Req} = R) ->
    case misultin_utility:header_get_value(K, Req:get(headers)) of
        false -> {undefined, R};
        V     -> {V, R}
    end.

-spec jsessionid(req()) -> {nonempty_string() | undefined, req()}.
jsessionid({cowboy, Req}) ->
    {C, Req2} = cowboy_http_req:cookie(<<"JSESSIONID">>, Req),
    case C of
        _ when is_binary(C) ->
            {binary_to_list(C), {cowboy, Req2}};
        undefined ->
            {undefined, {cowboy, Req2}}
    end;
jsessionid({misultin, Req} = R) ->
    C = Req:get_cookie_value("JSESSIONID", Req:get_cookies()),
    {C, R}.

-spec callback(req()) -> {nonempty_string() | undefined, req()}.
callback({cowboy, Req}) ->
    {CB, Req1} = cowboy_http_req:qs_val(<<"c">>, Req),
    case CB of
        undefined -> {undefined, {cowboy, Req1}};
        _         -> {binary_to_list(CB), {cowboy, Req1}}
    end;
callback({misultin, Req} = R) ->
    case proplists:get_value("c", Req:parse_qs()) of
        undefined ->
            {undefined, R};
        CB ->
            {CB, R}
    end.

%% --------------------------------------------------------------------------

-spec reply(non_neg_integer(), headers(), iodata(), req()) -> req().
reply(Code, Headers, Body, {cowboy, Req}) ->
    Body1 = iolist_to_binary(Body),
    {ok, Req1} = cowboy_http_req:reply(Code, enbinary(Headers), Body1, Req),
    {cowboy, Req1};
reply(Code, Headers, Body, {misultin, Req} = R) ->
    Req:respond(Code, Headers, Body),
    R.

-spec chunk_start(non_neg_integer(), headers(), req()) -> req().
chunk_start(Code, Headers, {cowboy, Req}) ->
    {ok, Req1} = cowboy_http_req:chunked_reply(Code, enbinary(Headers), Req),
    {cowboy, Req1};
chunk_start(_Code, Headers, {misultin, Req} = R) ->
    %% Untrap 'closed' message.
    Req:options([{comet, true}]),
    Req:chunk(head, Headers),
    R.

-spec chunk(iodata(), req()) -> {ok | error, req()}.
chunk(Chunk, {cowboy, Req} = R) ->
    case cowboy_http_req:chunk(Chunk, Req) of
        ok          -> {ok, R};
        {error, _E} -> {error, R}
                      %% This shouldn't happen too often, usually we
                      %% should catch tco socket closure before.
    end;
chunk(Chunk, {misultin, Req} = R) ->
    case Req:chunk(Chunk) of
        {stream_data, _} -> ok
                            %% Misultin just kills the process on
                            %% connection error.
    end,
    {ok, R}.

-spec chunk_end(req()) -> req().
chunk_end({cowboy, _Req} = R)  -> R;
chunk_end({misultin, Req} = R) -> Req:chunk(done),
                                  R.

enbinary(L) -> [{list_to_binary(K), list_to_binary(V)} || {K, V} <- L].


-spec hook_tcp_close(req()) -> req().
hook_tcp_close(R = {cowboy, Req}) ->
    {ok, T, S} = cowboy_http_req:transport(Req),
    T:setopts(S,[{active,once}]),
    R;
hook_tcp_close(R = {misultin, _Req}) ->
    R.

-spec unhook_tcp_close(req()) -> req().
unhook_tcp_close(R = {cowboy, Req}) ->
    {ok, T, S} = cowboy_http_req:transport(Req),
    T:setopts(S,[{active,false}]),
    R;
unhook_tcp_close(R = {misultin, _Req}) ->
    R.

-spec abruptly_kill(req()) -> req().
abruptly_kill(R = {cowboy, Req}) ->
    {ok, T, S} = cowboy_http_req:transport(Req),
    T:shutdown(S, read_write),
    R;
abruptly_kill(R = {misultin, _Req}) ->
    R.

