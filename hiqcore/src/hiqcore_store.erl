% @doc
% Hiqcore store.
%
% ==DBs==
% <ul>
% <li>hiqmail</li>
% <li>hiqmailStats</li>
% </ul>
% @end

%%
% WARNING: Always pass strings as binaries to mongodb backend.
%

-module(hiqcore_store).
-export([init/0, deinit/0, insert/1, update/1, delete/1, search/1, command/1, search_one/1, next/1, next/2]).

-include("hiqcore_domain_data.hrl").

-define(MONGO_HIQMAIL_DB, hiqmail).
-define(MONGO_CONN_MAX, 10).
-define(RECORD_INFO_TABLE, hiqcore_store_t1).

-define(INSERT_RECORD_INFO(Rec),
        true = ets:insert_new(?RECORD_INFO_TABLE, {Rec, record_info(fields, Rec)})).

-record(iter, {recordName, cursor}).

-opaque iterator() :: #iter{}.
-type domainRec() :: any().

init() ->
    {ok, _Pid} = mongo_sup:start_pool(?MODULE, ?MONGO_CONN_MAX, {hiqlib:get_opt(mongo_host), 27017}),
    ets:new(?RECORD_INFO_TABLE, [set, named_table, protected, {read_concurrency, true}]),
    ?INSERT_RECORD_INFO(user),
    ?INSERT_RECORD_INFO(mailRecipient),
    ?INSERT_RECORD_INFO(mailList),
    ?INSERT_RECORD_INFO(mailRecipientList),
    ?INSERT_RECORD_INFO(mailUserList),
    ?INSERT_RECORD_INFO(mailFilter),
    ?INSERT_RECORD_INFO(mailUserFilter),
    ?INSERT_RECORD_INFO(mailHeader),
    ?INSERT_RECORD_INFO(mailBody),
    ?INSERT_RECORD_INFO(mailAttachment),
    ?INSERT_RECORD_INFO(mailStatus),
    ?INSERT_RECORD_INFO(mailDelivery),
    ?INSERT_RECORD_INFO(mailCampaign),
    ?INSERT_RECORD_INFO(mailTag),
    ok.

deinit() ->
    ets:delete(?RECORD_INFO_TABLE),
    mongo_sup:stop_pool(?MODULE).

% @doc Send raw command to MongoDB. Only for DEBUG purposes.
command(Command) ->
    mongo:do(safe, master, get_connection(), ?MONGO_HIQMAIL_DB,
             fun () -> mongo:command(Command) end).

% @doc Insert and return a new record.
-spec insert(any()) -> domainRec().
insert(Rec) ->
    Col = element(1, Rec),
    [Res] = mongo:do(safe, master, get_connection(), ?MONGO_HIQMAIL_DB,
                   fun () -> mongo:insert(Col, [rec_to_doc(Rec)]) end),
    doc_to_rec(Res, Col).

% @doc Update record by Id.
-spec update(any()) -> any().
update(Rec) ->
    Col = element(1, Rec),
    Id = element(2, Rec),
    mongo:do(safe, master, get_connection(), ?MONGO_HIQMAIL_DB,
             fun () -> mongo:update(Col, {'_id', Id}, rec_to_doc(Rec)) end).

% @doc Delete record by Id.
-spec delete(any()) -> any().
delete(Rec) ->
    Col = element(1, Rec),
    Id = element(2, Rec),
    mongo:do(safe, master, get_connection(), ?MONGO_HIQMAIL_DB,
             fun () -> mongo:delete_one(Col, {'_id', Id}) end).

% @doc Search single matching record.
-spec search_one(any()) -> [] | domainRec().
search_one(Rec) ->
    Col = element(1, Rec),
    Res = mongo:do(safe, master, get_connection(), ?MONGO_HIQMAIL_DB,
                   fun () -> mongo:find_one(Col, rec_to_doc(Rec)) end),
    search_one_1(Res, Col).

% @private
search_one_1({}, _) ->
    [];
search_one_1({Doc}, Name) ->
    doc_to_rec(Doc, Name).

% @doc Search record.
-spec search(any()) -> iterator().
search(Rec) ->
    Col = element(1, Rec),
    Res = mongo:do(safe, master, get_connection(), ?MONGO_HIQMAIL_DB,
                   fun () -> mongo:find(Col, rec_to_doc(Rec)) end),
    #iter{cursor=Res, recordName=Col}.

% @doc Return next element.
-spec next(iterator()) -> [] | domainRec().
next(Iter) ->
    next_1(mongo_cursor:next(Iter#iter.cursor), Iter#iter.recordName).

% @private
next_1({}, _) ->
    [];
next_1({Doc}, Name) ->
    doc_to_rec(Doc, Name).

% @doc Return next `Limit' elements (discard the rest).
%      A new iterator is needed for more elements.
-spec next(iterator(), pos_integer()) -> [] | [domainRec()].
next(Iter, Limit) ->
    next_limit_1(mongo_cursor:take(Iter#iter.cursor, Limit), Iter#iter.recordName, []).

% @private
next_limit_1([], _, Acc) ->
    lists:reverse(Acc);
next_limit_1([Doc|Docs], Name, Acc) ->
    next_limit_1(Docs, Name, [doc_to_rec(Doc, Name)|Acc]).

% @private
get_connection() ->
    mongo_pool:get(?MODULE).

% @private
doc_to_rec(Doc, RecordName) ->
    [{_, Fields}] = ets:lookup(?RECORD_INFO_TABLE, RecordName),
    Fun = fun(F, Acc) -> erlang:append_element(Acc, bson:lookup(F, Doc, undefined)) end,
    lists:foldl(Fun, {RecordName}, Fields).

% @private
rec_to_doc(Record) ->
    [_ | Values] = tuple_to_list(Record),
    Fun = fun({F,V}, Acc) ->
                  case V /= undefined of
                      true -> erlang:append_element(erlang:append_element(Acc, F), V);
                      false -> Acc end end,
    lists:foldl(Fun, {}, lists:zip(get_rec_fields(Record), Values)).

% @private
get_rec_fields(Record) ->
    [{_, Fields}] = ets:lookup(?RECORD_INFO_TABLE, element(1, Record)),
    Fields.
