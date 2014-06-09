% @doc
% Hiqlib Logger Implementation.
% Logging module using disk_log to implement wrap logs.
%
% == Configuration ==
% The log level and name should be  defined in the app configuration file that
% includes this module. The default log level is info.
%
%   log_level = error | warning | info | debug
%
%   log_name = string()
%
% == Format ==
% The log format is,
%  ```<timestamp> " " <prefix> <level> ": " <message>'''
% Where timestamp is, 
%  ```<seconds> "." <microseconds>'''
% @end

-module(hiqlib_log_srv).
-behavior(gen_server).

-export([start_link/0, start_link/1, debug/1, debug/2, info/1, info/2, warning/1, warning/2,
         error/1, error/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(LOG_LEVELS, [{error,1}, {warning,2}, {info,3}, {debug,4}]).
-define(LOG_MAX, 5).
-define(LOG_SIZE, 1048576).

% @equiv start_link("")
start_link() ->
    start_link("").

-spec start_link(string()) -> any().
% @doc
% Spawn, link and register a new process locally. Use `Prefix' for every log message.
% @end
start_link(Prefix) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Prefix, []).

-spec debug(string()) -> ok | {error, Reason::term()}.
% @equiv debug(Msg, [])
debug(Msg) ->
    debug(Msg, []).

-spec debug(string(), [term()]) -> ok | {error, Reason::term()}.
% @doc
% Append `Args' to `Msg' and write it to the log level debug.
% @end
debug(Msg, Args) ->
    log(debug, Msg, Args).

-spec error(string()) -> ok | {error, Reason::term()}.
% @equiv error(Msg, [])
error(Msg) ->
    ?MODULE:error(Msg, []).

-spec error(string(), [term()]) -> ok | {error, Reason::term()}.
% @doc
% Append `Args' to `Msg' and write it to the log level error.
% @end
error(Msg, Args) ->
    log(error, Msg, Args).

-spec info(string()) -> ok | {error, Reason::term()}.
% @equiv info(Msg, [])
info(Msg) ->
    info(Msg, []).

-spec info(string(), [term()]) -> ok | {error, Reason::term()}.
% @doc
% Append `Args' to `Msg' and write it to the log level info.
% @end
info(Msg, Args) ->
    log(info, Msg, Args).

-spec warning(string()) -> ok | {error, Reason::term()}.
% @equiv warning(Msg, [])
warning(Msg) ->
    warning(Msg, []).

-spec warning(string(), [term()]) -> ok | {error, Reason::term()}.
% @doc
% Append `Args' to `Msg' and write it to the log level warning.
% @end
warning(Msg, Args) ->
    log(warning, Msg, Args).

% @private
log(Level, Msg, Args) ->
    log_1(msg_level(), proplists:get_value(Level, ?LOG_LEVELS), Level, Msg, Args).

% @private
log_1(MsgLevel, LIdx, Level, Msg, Args) when MsgLevel >= LIdx  ->
    TStr = lists:flatten(io_lib:format(Msg, Args)),
    gen_server:call(?MODULE, {log, time_str() ++ string:to_upper(atom_to_list(Level)) ++ ": " ++ TStr});
log_1(_MsgLevel, _LIdx, _Level, _Msg, _Args) ->
    ok.

% @private
time_str() ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time(TS),
    io_lib:format("~2w-~2..0w-~4w ~2w:~2..0w:~2..0w.~6..0w ",
                          [Day,Month,Year,Hour,Minute,Second,Micro]).

% @private
msg_level() ->
    case application:get_env(log_level) of
        {ok, L} -> proplists:get_value(L, ?LOG_LEVELS);
        _ -> proplists:get_value(info, ?LOG_LEVELS)
    end.

% @private
init(Prefix) ->
    {ok, Name} = application:get_env(log_name),
    case disk_log:open([{name, Name}, {type, wrap}, {format, external},
                        {size, {?LOG_SIZE, ?LOG_MAX}}]) of
        {ok, Log} ->
            disk_log:blog(Log, time_str() ++ "Starting log.\n"),
            {ok, {Log, Prefix}};
        Other ->
            {stop, Other}
    end.

% @private
terminate(_Reason, {Log, _Prefix}) ->
    disk_log:close(Log).

% @private
handle_call({log, Msg}, _From, {Log, Prefix}) ->
    {reply, disk_log:blog(Log, Prefix ++ Msg ++ "\n"), {Log, Prefix}}.

% @private
handle_cast(_Request, State) ->
    {noreply, State}.

% @private
handle_info(_Request, State) ->
    {noreply, State}.

% @private
code_change(_OldVer, State, _Extra) ->
    {ok, State}.
