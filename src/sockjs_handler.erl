-module(sockjs_handler).

-export([is_valid_ws/2]).
-export([dispatch_req/2, handle_req/2]).

-include("sockjs_internal.hrl").

%% --------------------------------------------------------------------------

-spec is_valid_ws(state(), req()) -> {boolean(), req()}.
is_valid_ws(State, Req) ->
    {Dispatch, Req2} = dispatch_req(State, Req),
    case Dispatch of
        {match, {_, websocket, _, _, _}} ->
            case valid_ws_request(State, Req2) of
                {false, Req3} ->
                    {false, Req3};
                {true, Req3} ->
                    {true, Req3}
            end;
        _Else ->
            {false, Req2}
    end.

-spec valid_ws_request(state(), req()) -> {boolean(), req()}.
valid_ws_request(_State, Req) ->
    {R1, Req1} = valid_ws_upgrade(Req),
    {R2, Req2} = valid_ws_connection(Req1),
    {R1 and R2, Req2}.

valid_ws_upgrade(Req) ->
    case sockjs_http:header('Upgrade', Req) of
        {undefined, Req2} ->
            {false, Req2};
        {V, Req2} ->
            case string:to_lower(V) of
                "websocket" ->
                    {true, Req2};
                _Else ->
                    {false, Req2}
            end
    end.

valid_ws_connection(Req) ->
    case sockjs_http:header('Connection', Req) of
        {undefined, Req2} ->
            {false, Req2};
        {V, Req2} ->
            Vs = [string:strip(T) ||
                     T <- string:tokens(string:to_lower(V), ",")],
            {lists:member("upgrade", Vs), Req2}
    end.

%% --------------------------------------------------------------------------

strip_prefix(LongPath, Prefix) ->
    {A, B} = lists:split(length(Prefix), LongPath),
    case Prefix of
        A    -> {ok, B};
        _Any -> {error, io_lib:format("Wrong prefix: ~p is not ~p", [A, Prefix])}
    end.


-type(dispatch_result() ::
        nomatch |
        {match, {send | recv | none , atom(),
                 server(), session(), list(atom())}} |
        {bad_method, list(atom())}).

-spec dispatch_req(state(), req()) -> {dispatch_result(), req()}.
dispatch_req(#state{prefix = Prefix}, Req) ->
    {Method, Req1} = sockjs_http:method(Req),
    {LongPath, Req2} = sockjs_http:path(Req1),
    {ok, PathRemainder} = strip_prefix(LongPath, Prefix),
    {dispatch(Method, PathRemainder), Req2}.

-spec dispatch(atom(), nonempty_string()) -> dispatch_result().
dispatch(Method, Path) ->
    lists:foldl(
      fun ({Match, MethodFilters}, nomatch) ->
              case Match(Path) of
                  nomatch ->
                      nomatch;
                  [Server, Session] ->
                      case lists:keyfind(Method, 1, MethodFilters) of
                          false ->
                              Methods = [ K ||
                                            {K, _, _, _} <- MethodFilters],
                              {bad_method, Methods};
                          {_Method, Type, A, Filters} ->
                              {match, {Type, A, Server, Session, Filters}}
                      end
              end;
          (_, Result) ->
              Result
      end, nomatch, filters()).

%% --------------------------------------------------------------------------

filters() ->
    OptsFilters = [h_sid, xhr_cors, cache_for, xhr_options_post],
    %% websocket does not actually go via handle_req/3 but we need
    %% something in dispatch/2
    [{t("/websocket"),               [{'GET',     none, websocket,      []}]},
     {t("/xhr_send"),                [{'POST',    recv, xhr_send,       [h_sid, xhr_cors]},
                                      {'OPTIONS', none, options,        OptsFilters}]},
     {t("/xhr"),                     [{'POST',    send, xhr_polling,    [h_sid, xhr_cors]},
                                      {'OPTIONS', none, options,        OptsFilters}]},
     {t("/xhr_streaming"),           [{'POST',    send, xhr_streaming,  [h_sid, xhr_cors]},
                                      {'OPTIONS', none, options,        OptsFilters}]},
     {t("/jsonp_send"),              [{'POST',    recv, jsonp_send,     [h_sid, expect_form]}]},
     {t("/jsonp"),                   [{'GET',     send, jsonp,          [h_sid, h_no_cache]}]},
     {t("/eventsource"),             [{'GET',     send, eventsource,    [h_sid, h_no_cache]}]},
     {t("/htmlfile"),                [{'GET',     send, htmlfile,       [h_sid, h_no_cache]}]},
     {p(""),                         [{'GET',     none, welcome_screen, []}]},
     {p("/iframe[0-9-.a-z_]*.html"), [{'GET',     none, iframe,         [cache_for]}]},
     {p("/chunking_test"),           [{'POST',    none, chunking_test,  [xhr_cors, expect_xhr]},
                                      {'OPTIONS', none, options,        OptsFilters}]},
     {p("/info"),                    [{'GET',     none, info_test,      [h_no_cache, xhr_cors]},
                                      {'OPTIONS', none, options,        [h_sid, xhr_cors, cache_for, xhr_options_get]}]}
    ].

p(S) -> fun (Path) -> re(Path, "^" ++ S ++ "[/]?\$") end.
t(S) -> fun (Path) -> re(Path, "^/([^/.]+)/([^/.]+)" ++ S ++ "[/]?\$") end.

re(Path, S) ->
    case re:run(Path, S, [{capture, all_but_first, list}]) of
        nomatch                    -> nomatch;
        {match, []}                -> [dummy, dummy];
        {match, [Server, Session]} -> [Server, Session]
    end.

%% --------------------------------------------------------------------------

-spec handle_req(state(), req()) -> req().
handle_req(State, Req) ->
    {Dispatch, Req1} = dispatch_req(State, Req),
    handle(Dispatch, State, Req1).

handle(nomatch, _State, Req) ->
    sockjs_http:reply(404, [], "", Req);

handle({bad_method, Methods}, _State, Req) ->
    MethodsStr = string:join([atom_to_list(M) || M <- Methods],
                             ", "),
    H = [{"Allow", MethodsStr}],
    sockjs_http:reply(405, H, "", Req);

handle({match, {Type, Action, _Server, Session, Filters}},
       State = #state{callback = Callback}, Req) ->
    {Headers, Req2} = lists:foldl(
                        fun (Filter, {Headers0, Req1}) ->
                                sockjs_filters:Filter(Req1, Headers0)
                        end, {[], Req}, Filters),
    case Type of
        send ->
                sockjs_session:maybe_create(Session, Callback),
                sockjs_action:Action(Req2, Headers, State, Session);
        recv ->
            try
                sockjs_action:Action(Req2, Headers, Session, Session)
            catch throw:no_session ->
                    {H, Req3} = sockjs_filters:h_sid(Req2, []),
                    sockjs_http:reply(404, H, "", Req3)
            end;
        none ->
            sockjs_action:Action(Req2, Headers, State)
    end.
