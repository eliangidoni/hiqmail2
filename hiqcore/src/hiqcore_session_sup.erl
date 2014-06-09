% @doc
% Hiqcore session supervisor.
% This supervisor starts the following processes,
% <ul>
%       <li>{@link hiqcore_filter_srv.}</li>
%       <li>{@link hiqcore_event_srv.}</li>
%       <li>{@link hiqcore_user_srv.}</li>
% </ul>
% @end

-module(hiqcore_session_sup).
-behavior(supervisor).

-export([start_link/0, start_event_srv/2, start_filter_srv/2, start_user_srv/2]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

% @private
init(_Args) ->
    {ok, {{one_for_all, 0, 1}, []}}.

% @doc Start filter server.
-spec start_filter_srv(pid(), any()) -> pid().
start_filter_srv(Sup, Sid) ->
    Spec = {hiqcore_filter_srv, {hiqcore_filter_srv, start_link, [Sid]},
            permanent, brutal_kill, worker, [hiqcore_filter_srv]},
    {ok, Pid} = supervisor:start_child(Sup, Spec),
    Pid.

% @doc Start user server.
-spec start_user_srv(pid(), any()) -> pid().
start_user_srv(Sup, Sid) ->
    Spec = {hiqcore_user_srv, {hiqcore_user_srv, start_link, [Sid]},
            permanent, brutal_kill, worker, [hiqcore_user_srv]},
    {ok, Pid} = supervisor:start_child(Sup, Spec),
    Pid.

% @doc Start event server.
-spec start_event_srv(pid(), any()) -> pid().
start_event_srv(Sup, Sid) ->
    Spec = {hiqcore_event_srv, {hiqcore_event_srv, start_link, [Sid]},
            permanent, brutal_kill, worker, [hiqcore_event_srv]},
    {ok, Pid} = supervisor:start_child(Sup, Spec),
    Pid.
