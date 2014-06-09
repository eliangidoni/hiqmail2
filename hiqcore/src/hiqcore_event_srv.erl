% @doc
% Hiqcore event server.
% @end

-module(hiqcore_event_srv).
-behavior(gen_server).

-export([start_link/1, notify/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Sid) ->
    gen_server:start_link(?MODULE, [Sid], []).

notify(Event) ->
    gen_server:cast(?MODULE, {notify, Event}).

% @private
init([Sid]) ->
    {ok, [Sid]}.

% @private
terminate(_Reason, _State) ->
    ok.

% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% @private
handle_cast({notify, Event}, State) ->
    handle_notify(Event, State).

% @private
handle_notify(_Event, State) ->
    {noreply, State}.

% @private
handle_info(_Req, State) ->
    {noreply, State}.

% @private
code_change(_OldVer, State, _Extra) ->
    {ok, State}.
