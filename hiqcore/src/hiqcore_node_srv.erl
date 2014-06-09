% @doc
% Hiqcore Node.
% This module implements the main functions of the Hiqmail Core Node (heartbeat, start/stop commands, etc).
% @end

-module(hiqcore_node_srv).
-behavior(gen_server).

-export([start_link/0]).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-define(HTTPD_CONF,[{modules, [mod_esi, mod_log, mod_alias, mod_get]},
                    {error_log, "error.log"},
                    {security_log, "security.log"},
                    {transfer_log, "transfer.log"},
                    {mime_types, [{"html","text/html"}, {"css","text/css"}, {"js","application/x-javascript"}]}]).

-record(state, {httpdPid, httpdOptions, corePids=[], timer}).

-define(NOTIFY_PERIOD_MS, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @private
init([]) ->
    hiqcore_store:init(),
    Options = hiqlib:key_insert([{server_root, hiqlib:get_opt(httpd_doc_dir)},
                                 {document_root, hiqlib:get_opt(httpd_doc_dir)},
                                 {port, hiqlib:get_opt(httpd_port)},
                                 {server_name, hiqlib:get_opt(httpd_name)},
                                 {bind_address, hiqlib:get_opt(httpd_addr)},
                                 {erl_script_alias, {"/api", hiqlib:get_opt(httpd_mod_list)}}],
                                ?HTTPD_CONF),
    true = code:add_patha(hiqlib:get_opt(httpd_mod_dir)),
    {ok, Pid} = inets:start(httpd, Options),
    Timer = erlang:send_after(?NOTIFY_PERIOD_MS, self(), filter),
    {ok, #state{httpdPid=Pid, httpdOptions=Options, timer=Timer}}.

% @private
terminate(Reason, State) ->
    hiqlib_log_srv:debug("Terminating node, reason: ~p", [Reason]),
    hiqcore_store:deinit(),
    inets:stop(httpd, State#state.httpdPid).

% @private
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% @private
handle_cast(_Req, State) ->
    {noreply, State}.

% @private
handle_info(filter, #state{timer=OldTimer}=State) ->
    hiqlib_log_srv:debug("Filtering mails"),
    erlang:cancel_timer(OldTimer),
    Timer = erlang:send_after(?NOTIFY_PERIOD_MS, self(), filter),
    {noreply, State#state{timer=Timer}};
handle_info(_Req, State) ->
    {noreply, State}.

% @private
code_change(_OldVer, State, _Extra) ->
    {ok, State}.
