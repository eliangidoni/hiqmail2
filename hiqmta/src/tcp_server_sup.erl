% @doc
% Hiqmta Bounce Server Supervisor.
% @end

-module(tcp_server_sup).
-behavior(supervisor).

-export([start_link/0, start_bounceserv/0]).
-export([init/1]).

-define(TCP_LISTEN_PORT, 1235).
-define(TCP_LISTEN_OPT, [{active, once}]).
-define(TCP_ACCEPT_CHILDS, 10).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% @private
init(_Args) ->
    {ok, ListenSock} = gen_tcp:listen(?TCP_LISTEN_PORT, ?TCP_LISTEN_OPT),
    spawn_link(fun() -> [start_bounceserv() || _ <- lists:seq(1, ?TCP_ACCEPT_CHILDS)] end),
    {ok, {{simple_one_for_one, 0, 1},
          [{tcp_server, {tcp_server, start_link, [ListenSock]},
            temporary, brutal_kill, worker, [tcp_server]}]}}.

-spec start_new() -> ok.
% @doc
% Start a new {@link hiqmta_bounceserv.}.
% @end
start_new() ->
    {ok, _Pid} = supervisor:start_child(?MODULE, []),
    ok.
