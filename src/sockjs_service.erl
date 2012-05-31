%% ***** BEGIN LICENSE BLOCK *****
%% Copyright (c) 2011-2012 VMware, Inc.
%%
%% For the license see COPYING.
%% ***** END LICENSE BLOCK *****

-module(sockjs_service).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {sockjs_init, 2},
     {sockjs_handle, 3},
     {sockjs_terminate, 2},
     {sockjs_info, 3}
    ];

behaviour_info(_Other) ->
    undefined.
