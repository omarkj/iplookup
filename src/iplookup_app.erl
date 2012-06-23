%%% -*- mode: erlang -*-
-module(iplookup_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    ok = application:start(inets),
    ok = application:start(mnesia),
    ok = application:start(cowboy),
    ok = application:start(iplookup).

start(_StartType, _StartArgs) ->
    iplookup_sup:start_link().

stop(_State) ->
    ok.
