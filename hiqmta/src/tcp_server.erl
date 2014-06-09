% @doc
% Hiqmta Bounce Server.
% @end

-module(tcp_server).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TCP_CLIENT_OPT, [binary, {keepalive, true}, {nodelay, true}, {packet,4}]).

start_link(ListenSock) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

% @private
init([ListenSock]) ->
    gen_server:cast(self(), {go, ListenSock}),
    {ok, []}.

% @private
terminate(_Reason, []) ->
    ok;
terminate(_Reason, ConnSock) ->
    gen_tcp:close(ConnSock),
    ok.

% @private
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

% @private
handle_cast({go, ListenSock}, []) ->
    case gen_tcp:accept(ListenSock) of
        {ok, S} ->
            % Start a new server to call accept().
            tcp_server_sup:start_new(),
            inet:setopts(S, ?TCP_CLIENT_OPT),
            {noreply, S};
        Other ->
            hiqlib_logger:log(warning, "accept failed: ", [Other]),
            exit(error_accept)
    end.

% @private
handle_info({tcp, S, Data}, State) ->
    case gen_tcp:send(S, proc_request(Data)) of
        ok ->
            inet:setopts(S, [{active, once}]),
            {noreply, State};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_info({tcp_closed, _S}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _S, Reason}, State) ->
    {stop, Reason, State}.

% @private
code_change(_OldVer, State, _Extra) ->
    {ok, State}.

% @private
proc_request(Data) ->
    ok.
