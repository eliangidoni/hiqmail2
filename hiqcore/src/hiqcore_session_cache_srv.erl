% @doc
% Hiqcore session cache.
% @end

-module(hiqcore_session_cache_srv).
-behavior(gen_server).

-export([start_link/0, get/1]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("hiqcore_session.hrl").

-record(state, {sidTable, sessionTable, sessionCount}).

-define(SID_TABLE, hiqcore_session_cache_srv_t1).
-define(SESSION_TABLE, hiqcore_session_cache_srv_t2).
-define(SESSION_MAX, 10).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @doc Gets or adds new session.
-spec get(binary()) -> #sessionInfo{} | limit.
get(Sid) ->
    get_1(ets:lookup(?SESSION_TABLE, Sid), Sid).

% @private
get_1([Value], _Sid) ->
    Value;
get_1(_, Sid) ->
    gen_server:call(?MODULE, {insert, Sid}).

% @private
init([]) ->
    Sessions = ets:new(?SESSION_TABLE,
                       [set, named_table, protected,
                        {keypos, #sessionInfo.sid},
                        {read_concurrency, true}]),
    Sids = ets:new(?SID_TABLE, [set, protected, {write_concurrency, true}]),
    {ok, #state{sidTable=Sids, sessionTable=Sessions, sessionCount=0}}.

% @private
terminate(_Reason, State) ->
    ets:delete(State#state.sidTable),
    ets:delete(State#state.sessionTable).

% @private
handle_call({insert, Sid}, _From, State) ->
    handle_insert(ets:lookup(State#state.sessionTable, Sid), State, Sid).

% @private
handle_insert([Value], State, _Sid) ->
    {reply, Value, State};
handle_insert(_, State, _Sid) when State#state.sessionCount >= ?SESSION_MAX ->
    {reply, limit, State};
handle_insert(_, #state{sessionCount=Count}=State, Sid) ->
    {reply, start_monitor_session(State, Sid), State#state{sessionCount=Count+1}}.

% @private
handle_cast(_Req, State) ->
    {noreply, State}.

% @private
handle_info({'DOWN', Ref, _Type, Pid, _Info}, State) ->
    handle_down(State, Ref, Pid).

% @private
handle_down(State, Ref, Pid) ->
    [{Pid, Sid}] = ets:lookup(State#state.sidTable, Pid),
    true = ets:delete(State#state.sessionTable, Sid),
    true = ets:delete(State#state.sidTable, Pid),
    true = demonitor(Ref),
    hiqlib_log_srv:debug("Restarting died session: ~p, Sid: ~p", [Pid, Sid]),
    start_monitor_session(State, Sid),
    {noreply, State}.

% @private
code_change(_OldVer, State, _Extra) ->
    {ok, State}.

% @private
start_monitor_session(State, Sid) ->
    Value = hiqcore_sessions_sup:start_session(Sid),
    Value2 = Value#sessionInfo{sid=Sid},
    Sup = Value2#sessionInfo.ssup,
    true = ets:insert_new(State#state.sidTable, {Sup, Sid}),
    true = ets:insert_new(State#state.sessionTable, Value2),
    monitor(process, Sup),
    Value2.
