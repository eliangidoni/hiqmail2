% @doc
% Hiqcore Application Behavior.
% This is the application entry point, calls {@link hiqcore_sup:start_link/0.}.
% @end

-module(hiqcore_app).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    hiqcore_sup:start_link().

stop(_State) ->
    ok.
