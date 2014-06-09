% @doc
% Hiqcore Root Supervisor.
% This supervisor starts the following processes,
% <ul>
%       <li>{@link hiqlib_log_srv.}</li>
%       <li>{@link hiqcore_session_cache_srv.}</li>
%       <li>{@link hiqcore_evaluator_sup.}</li>
%       <li>{@link hiqcore_sessions_sup.}</li>
%       <li>{@link hiqcore_node_srv.}</li>
% </ul>
% @end

-module(hiqcore_sup).
-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

% @private
init(_Args) ->
    {ok, {{one_for_one, 1, 60},

          [{hiqlib_log_srv, {hiqlib_log_srv, start_link, []},
            permanent, brutal_kill, worker, [hiqlib_log_srv]},

           {hiqcore_session_cache_srv, {hiqcore_session_cache_srv, start_link, []},
            permanent, brutal_kill, worker, [hiqcore_session_cache_srv]},

           {hiqcore_evaluator_sup, {hiqcore_evaluator_sup, start_link, []},
            permanent, infinity, supervisor, [hiqcore_evaluator_sup]},

           {hiqcore_sessions_sup, {hiqcore_sessions_sup, start_link, []},
            permanent, infinity, supervisor, [hiqcore_sessions_sup]},

           {hiqcore_node_srv, {hiqcore_node_srv, start_link, []},
            permanent, brutal_kill, worker, [hiqcore_node_srv]}]}}.
