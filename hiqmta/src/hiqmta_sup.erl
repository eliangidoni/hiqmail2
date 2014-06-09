% @doc
% Hiqmta Root Supervisor.
% This supervisor starts the following processes,
% <ul>
%       <li>{@link hiqlib_logger.}</li>
%       <li>{@link hiqmta_bounceserv_sup.}</li>
%       <li>{@link hiqmta_db_sup.}</li>
%       <li>{@link hiqmta_core.}</li>
% </ul>
% @end

-module(hiqmta_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

% @private
init(_Args) ->
    {ok, {{one_for_all, 1, 60},

          [{hiqlib_logger, {hiqlib_logger, start_link, []},
            permanent, brutal_kill, worker, [hiqlib_logger]},

           {hiqmta_bounceserv_sup, {hiqmta_bounceserv_sup, start_link, []},
            permanent, infinity, supervisor, [hiqmta_bounceserv_sup]},

           {hiqmta_db_sup, {hiqmta_db_sup, start_link, []},
            permanent, infinity, supervisor, [hiqmta_db_sup]},

           {hiqmta_core, {hiqmta_core, start_link, []},
            permanent, brutal_kill, worker, [hiqmta_core]}]}}.
