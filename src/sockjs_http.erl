-module(sockjs_http).

-export([path/1, method/1, body/1, body_qs/1, jsessionid/1, callback/1,
         header/2, reply/4, chunk_start/3, chunk/2, chunk_end/1]).

-export([misultin_ws_loop/2]).

%% --------------------------------------------------------------------------

path({cowboy, Req})       -> {Path, Req1} = cowboy_http_req:raw_path(Req),
                             {binary_to_list(Path), {cowboy, Req1}};
path({misultin, Req} = R) -> case element(1, Req) of
                                 misultin_ws -> {Req:get(path), R};
                                 _           -> {abs_path, Path} = Req:get(uri),
                                                {Path, R}
                             end.

method({cowboy, Req})       -> {Method, Req1} = cowboy_http_req:method(Req),
                               {Method, {cowboy, Req1}};
method({misultin, Req} = R) -> {Req:get(method), R}.

body({cowboy, Req})       -> {ok, Body, Req1} = cowboy_http_req:body(Req),
                             {Body, {cowboy, Req1}};
body({misultin, Req} = R) -> {Req:get(body), R}.

body_qs(R) ->
    case header('Content-Type', R) of
        "text/plain" ->
            body(R);
        _ ->
            %% Assume application/x-www-form-urlencoded by default
            body_qs2(R)
    end.

body_qs2({cowboy, Req})       -> {BodyQS, Req1} = cowboy_http_req:body_qs(Req),
                                 {proplists:get_value(<<"d">>, BodyQS),
                                  {cowboy, Req1}};
body_qs2({misultin, Req} = R) -> {proplists:get_value("d", Req:parse_post()), R}.

%% TODO fix Req mutation for these two
jsessionid({cowboy, Req}) ->
    {C, _} = cowboy_http_req:cookie(<<"JSESSIONID">>, Req),
    C;
jsessionid({misultin, Req}) ->
    Req:get_cookie_value('JSESSIONID', Req:get_cookies()).

callback({cowboy, Req}) ->
    {CB, Req1} = cowboy_http_req:qs_val(<<"c">>, Req),
    {CB, {cowboy, Req1}};
callback({misultin, Req} = R) ->
    CB = list_to_binary(proplists:get_value("c", Req:parse_qs())),
    {CB, R}.

header(K, {cowboy, Req})->
    {H, _} = cowboy_http_req:header(K, Req),
    V = case H of
            undefined ->
                {H1, _} = cowboy_http_req:header(atom_to_binary(K, utf8), Req),
                H1;
            _ -> H
        end,
    case V of
        undefined -> undefined;
        _         -> binary_to_list(V)
    end;

header(K, {misultin, Req}) ->
    case misultin_utility:header_get_value(K, Req:get(headers)) of
        false -> undefined;
        V -> V
    end.

reply(Code, Headers, Body, {cowboy, Req}) when is_list(Body) ->
    reply(Code, Headers, list_to_binary(Body), {cowboy, Req});
reply(Code, Headers, Body, {cowboy, Req}) ->
    {ok, Req1} = cowboy_http_req:reply(Code, enbinary(Headers), Body, Req),
    {cowboy, Req1};
reply(Code, Headers, Body, {misultin, Req} = R) ->
    Req:respond(Code, Headers, Body),
    R.

chunk_start(Code, Headers, {cowboy, Req}) ->
    {ok, Req1} = cowboy_http_req:chunked_reply(Code, enbinary(Headers), Req),
    {cowboy, Req1};
chunk_start(_Code, Headers, {misultin, Req} = R) ->
    Req:chunk(head, Headers),
    R.

chunk(Chunk, {cowboy, Req})   -> case cowboy_http_req:chunk(Chunk, Req) of
                                     ok -> ok;
                                     {error, _} -> error
                                 end;
chunk(Chunk, {misultin, Req}) -> case Req:chunk(Chunk) of
                                     {stream_data, _} -> ok
                                     %% Misultin just kills the
                                     %% process on connection error.
                                 end.

chunk_end({cowboy, _Req} = R)  -> R;
chunk_end({misultin, Req} = R) -> Req:chunk(done),
                                  R.

enbinary(L) -> [{list_to_binary(K), list_to_binary(V)} || {K, V} <- L].

%% --------------------------------------------------------------------------

-define(WS_MODULE, sockjs_ws).

misultin_ws_loop(Ws, Receive) ->
    Ws:send(["o"]),
    Self = {?WS_MODULE, Ws, misultin},
    Receive(Self, init),
    misultin_ws_loop0(Ws, Receive, Self).

misultin_ws_loop0(Ws, Receive, Self) ->
    receive
        {browser, Data} ->
            Decoded = sockjs_util:decode(Data),
            Receive(Self, {recv, Decoded}),
            misultin_ws_loop0(Ws, Receive, Self);
        closed ->
            Receive(Self, closed),
            closed;
        Msg ->
            Receive(Self, {info, Msg}),
            misultin_ws_loop0(Ws, Receive, Self)
    end.
