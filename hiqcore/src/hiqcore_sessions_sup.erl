% @doc
% Hiqcore sessions supervisor.
% @end

-module(hiqcore_sessions_sup).
-behavior(supervisor).

-export([start_link/0, start_session/1]).

-export([init/1]).

-include("hiqcore_session.hrl").

% @doc Spawn, link and register a new process locally.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @doc Spaws new session.
-spec start_session(any()) -> #sessionInfo{}.
start_session(Sid) ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    ESrv = hiqcore_session_sup:start_event_srv(Pid, Sid),
    FSrv = hiqcore_session_sup:start_filter_srv(Pid, Sid),
    USrv = hiqcore_session_sup:start_user_srv(Pid, Sid),
    #sessionInfo{ssup=Pid, eventSrv=ESrv, filterSrv=FSrv, userSrv=USrv}.

% @private
init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{hiqcore_session_sup, {hiqcore_session_sup, start_link, []},
            temporary, infinity, supervisor, [hiqcore_session_sup]}]}}.
