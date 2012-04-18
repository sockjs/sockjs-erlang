%% ***** BEGIN LICENSE BLOCK *****
%% Copyright (c) 2011-2012 VMware, Inc.
%%
%% For the license see COPYING.
%% ***** END LICENSE BLOCK *****

-module(sockjs_json).

-export([encode/1, decode/1]).

%% --------------------------------------------------------------------------

-spec encode(any()) -> iodata().
encode(Thing) ->
    mochijson2_fork:encode(Thing).

-spec decode(iodata()) -> {ok, any()} | {error, any()}.
decode(Encoded) ->
    try mochijson2_fork:decode(Encoded) of
        V -> {ok, V}
    catch
        _:E -> {error, E}
    end.
