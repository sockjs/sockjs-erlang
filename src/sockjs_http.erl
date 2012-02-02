-module(sockjs_http).

-export([path/1, method/1, header/2, jsessionid/1]).
-export([reply/4, chunk_start/3, chunk/2, chunk_end/1]).

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
method({misultin, Req} = R) -> {Req:get(method), R}.


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
    Req:chunk(head, Headers),
    R.

-spec chunk(iodata(), req()) -> {ok | error, req()}.
chunk(Chunk, {cowboy, Req}) ->
    case cowboy_http_req:chunk(Chunk, Req) of
        ok         -> {ok, Req};
        {error, _} -> {error, Req}
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
