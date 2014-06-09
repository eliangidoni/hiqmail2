% @doc
% Hiqfilter Tables.
% This module contains functions to bind processors and evaluators to rulesets and mails.
% @end

-module(hiqcore_filter_tables).

-export([init/0, find_rules/1, remove_rules/1, update_mails/2, get_mails/1, bind/1, unbind/1,
         any_processor/1, find_processor/1, try_bind/1, deinit/0]).

-spec init() -> ok.
% @doc
% Initialize filter tables.
% @end
init() ->
    ets:new(rules_table(), [named_table, public, {write_concurrency, true}]),
    ets:new(mkeys_table(), [named_table, bag, public, {write_concurrency, true}]),
    ets:new(procs_table(), [named_table, public, {write_concurrency, true}]),
    ok.

-spec deinit() -> ok.
% @doc
% De-initialize filter tables.
% @end
deinit() ->
    ok.

% @private
procs_table() -> list_to_atom(atom_to_list(?MODULE) ++ "_procs").

% @private
mkeys_table() -> list_to_atom(atom_to_list(?MODULE) ++ "_mkeys").

% @private
rules_table() -> list_to_atom(atom_to_list(?MODULE) ++ "_rules").

-spec find_rules(string()) -> term() | none.
% @doc
% Find rules for `MailId'.
% @end
find_rules(MailId) ->
    find_rules_1(catch ets:lookup(rules_table(), MailId)).

% @private
find_rules_1({'EXIT', _}) -> none;
find_rules_1([{_Key, Val}]) -> Val;
find_rules_1(_Other) -> none.

-spec remove_rules(string()) -> ok.
% @doc
% Remove rules for `MailId'.
% @end
remove_rules(MailId) ->
    [RuleSetId, _Id] = string:tokens(MailId, "-"),
    true = ets:delete_object(mkeys_table(), {RuleSetId, MailId}),
    true = ets:delete(rules_table(), MailId),
    ok.

-spec update_mails(any(), any()) -> ok.
% @doc
% Update tables with new mails for evaluation.
% @end
update_mails(ById, ByRuleSetId) ->
    true = ets:insert(rules_table(), ById),
    true = ets:insert(mkeys_table(), ByRuleSetId),
    ok.

-spec get_mails(any()) -> [string()].
% @doc
% Return mail IDs for `RuleSetId'.
% @end
get_mails(RuleSetId) ->
    get_mails_1(catch ets:lookup(mkeys_table(), RuleSetId)).

% @private
get_mails_1({'EXIT', _}) -> [];
get_mails_1([]) -> [];
get_mails_1(Values) -> lists:map(fun({_K,V}) -> V end, Values).

-spec try_bind(string()) -> {ok, Id::string()} | {error, Reason::string()}.
% @doc
% Try to bind process to `RuleSetId'.
% @end
try_bind(RuleSetId) ->
    try_bind_1(catch ets:insert_new(procs_table(), {RuleSetId, self()}), RuleSetId).

% @private
try_bind_1({'EXIT', _}, _RuleSetId) -> {error, "table insert failed"};
try_bind_1(false, _RuleSetId) -> {error, "key duplicate in table"};
try_bind_1(true, RuleSetId) -> {ok, RuleSetId}.

-spec bind(string()) -> ok.
% @doc
% Bind process to `RuleSetId'.
% @end
bind(RuleSetId) ->
    true = ets:insert(procs_table(), [{RuleSetId, self()}]),
    ok.

-spec unbind(string()) -> ok.
% @doc
% Unbind process from `RuleSetId'.
% @end
unbind(RuleSetId) ->
    true = ets:delete(procs_table(), RuleSetId),
    ok.

-spec find_processor(string()) -> {ok, pid()} | none.
% @doc
% Find processor for `RuleSetId'.
% @end
find_processor(RuleSetId) ->
    find_processor_1(catch ets:lookup(procs_table(), RuleSetId)).

% @private
find_processor_1({'EXIT', _}) -> none;
find_processor_1([{_Key, Val}]) -> {ok, Val};
find_processor_1(_Other) -> none.

-spec any_processor(string()) -> true | false.
% @doc
% Return true if a live processor for `RuleSetId' exists, false otherwise.
% @end
any_processor(RuleSetId) ->
    (find_processor(RuleSetId) /= none).
