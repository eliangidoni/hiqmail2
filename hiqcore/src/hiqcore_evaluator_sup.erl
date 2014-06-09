% @doc
% Hiqcore evaluator supervisor.
% @end

-module(hiqcore_evaluator_sup).
-behavior(supervisor).

-export([start_link/0, start_evaluator/0]).

-export([init/1]).

% @doc Spawn, link and register a new process locally.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @doc
-spec start_evaluator() -> pid().
start_evaluator() ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    Pid.

% @private
init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{hiqcore_evaluator_srv, {hiqcore_evaluator_srv, start_link, []},
            temporary, brutal_kill, worker, [hiqcore_evaluator_srv]}]}}.
