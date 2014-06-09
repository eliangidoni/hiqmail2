% @doc
% Hiqstore Mail Buffer.
% Buffer implementation, uses mnesia.
% @end

-module(hiqcore_store_buffer).

-export([put/3, parse_rules/1, init/0, get_rules/1, get_texts/1, foreach_rule/1, deinit/0]).

-record(hiqcoreMail, {id, status, value}).

-spec init() -> ok.
% @doc
% Initialize buffer.
% @end
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(hiqcoreMail, [{disc_copies, [node()]},
                                      {index, [status]},
                                      {attributes, record_info(fields, hiqcoreMail)}]),
    ok.

-spec deinit() -> ok.
% @doc
% De-initialize buffer.
% @end
deinit() ->
    mnesia:stop().

-spec put(string(), hiqlib:rules(), binary()) -> {Id::term(), ok} |
                                                 {Id::term(), Reason::string()}.
% @doc
% Store (or update) a mail.
% @end
put(Id, Rules, Text) ->
    Mail = #hiqcoreMail{id=Id, status=queued, value=hiqlib:mail_new(Rules, Text)},
    Trans = fun() ->
                    mnesia:write(Mail),
                    {Id, ok}
            end,
    {atomic, Res} = mnesia:transaction(Trans),
    Res.

-spec parse_rules(list()) -> {Id::string(), hiqlib:rules()} | absent_id.
% @doc
% Returns rule object from parameter list.
% @end
parse_rules(Params) ->
    Rules = filter_rules(Params),
    parse_rules_1(proplists:lookup(<<"#id">>, Rules), Rules).

% @private
filter_rules(Params) ->
    Fun = fun({Key, _Val}) -> filter_rules_1(catch (lists:nth(1, binary_to_list(Key)) == $#)) end,
    lists:filter(Fun, Params).

% @private
filter_rules_1({'EXIT', _}) -> false;
filter_rules_1(Other) -> Other.

% @private
parse_rules_1({_Key, Id}, Rules) -> {binary_to_list(Id), Rules};
parse_rules_1(none, _Rules) -> absent_id.

-spec get_rules([term()]) -> [hiqlib:rules()].
% @doc
% Return mail rules for `Ids'.
% @end
get_rules(Ids) ->
    Trans = fun() ->
                    Fun = fun(Id, Acc) -> get_rules_1(mnesia:read({hiqcoreMail, Id}), Acc) end,
                    lists:foldl(Fun, [], Ids)
            end,
    {atomic, Res} = mnesia:transaction(Trans),
    Res.

% @private
get_rules_1([], Acc) -> Acc;
get_rules_1([Mail], Acc) -> [hiqlib:mail_rules(Mail#hiqcoreMail.value) | Acc].

-spec get_texts([term()]) -> [binary()].
% @doc
% Return mail texts for `Ids'.
% @end
get_texts(Ids) ->
    Trans = fun() ->
                    Fun = fun(Id, Acc) -> get_texts_1(mnesia:wread({hiqcoreMail, Id}), Acc) end,
                    lists:foldl(Fun, [], Ids)
            end,
    {atomic, Res} = mnesia:transaction(Trans),
    Res.

% @private
get_texts_1([], Acc) -> Acc;
get_texts_1([Mail], Acc) ->
    mnesia:delete({hiqcoreMail, Mail#hiqcoreMail.id}),
    [hiqlib:mail_text(Mail#hiqcoreMail.value) | Acc].

-spec foreach_rule(fun((Rule::term()) -> any())) -> ok.
% @doc
% Call `Fun' for every queued mail rules.
% @end
foreach_rule(Fun) ->
    Trans = fun() ->
                    Mails = mnesia:match_object(#hiqcoreMail{id='_', status=queued, value='_'}),
                    UpdateFun = fun(M, Acc) -> mnesia:write(M#hiqcoreMail{status=filtered}),
                                               [hiqlib:mail_rules(M#hiqcoreMail.value) | Acc] end,
                    lists:foldl(UpdateFun, [], Mails)
            end,
    {atomic, Res} = mnesia:transaction(Trans),
    lists:foreach(Fun, Res).
