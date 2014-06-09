% @doc
% Hiqcore evaluator server.
% @end

-module(hiqcore_evaluator_srv).
-behavior(gen_server).

-export([start_link/0, evaluate/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

% @doc Evaluate deliveries and notify to hiqmta nodes.
-spec evaluate(pid(), [any()], [any()], [any()]) -> ok.
evaluate(Pid, Rules, Deliveries, MTAs) ->
    gen_server:cast(Pid, {evaluate, Rules, Deliveries, MTAs}).

% @private
init([]) ->
    {ok, []}.

% @private
terminate(_Reason, _State) ->
    ok.

% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% @private
handle_cast({evaluate, Rules, Deliveries, MTAs}, State) ->
    handle_evaluate(Rules, Deliveries, MTAs, State).

% @private
handle_evaluate(_Rules, _Deliveries, _MTAs, State) ->
    {stop, normal, State}.

% @private
handle_info(_Req, State) ->
    {noreply, State}.

% @private
code_change(_OldVer, State, _Extra) ->
    {ok, State}.
