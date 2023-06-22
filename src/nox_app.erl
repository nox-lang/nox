%%%-------------------------------------------------------------------
%% @doc nox public API
%% @end
%%%-------------------------------------------------------------------

-module(nox_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nox_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
