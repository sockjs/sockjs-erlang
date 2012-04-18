%% ***** BEGIN LICENSE BLOCK *****
%% Copyright (c) 2011-2012 VMware, Inc.
%%
%% For the license see COPYING.
%% ***** END LICENSE BLOCK *****

-module(sockjs_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    sockjs_session:init(),
    sockjs_session_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.
