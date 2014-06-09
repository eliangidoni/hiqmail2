-module(hiqmanager).
-behavior(application).

-export([start/2, stop/1]).

start(Type, Args) ->
    hiqmanager_sup:start_link().

stop(State) ->
    ok.
