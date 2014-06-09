% @doc
% Hiqlib Helper Functions.
% @end

-module(hiqlib).

-export([mail_new/2, mail_text/1, mail_rules/1]).
-export([key_replace/2, key_insert/2, get_opt/1, repstr/3, repstr/4, tpl_parse/3, tpl_render/2, strip/1]).

-record(mailData, {rules = dict:new(),
                   text = <<>>}).

-record(tpl, {tagIndices, text, size}).

-type rules()::list({Name::binary(), Value::binary()}).
-opaque mail()::#mailData{}.
-opaque tplInfo()::#tpl{}.

% @doc Create a new mail.
-spec mail_new(rules(), binary()) -> mail().
mail_new(Rules, Text) ->
    #mailData{rules=Rules, text=Text}.

% @doc Return mail rules.
-spec mail_rules(mail()) -> rules().
mail_rules(Mail) ->
    Mail#mailData.rules.

% @doc Return mail text
-spec mail_text(mail()) -> binary().
mail_text(Mail) ->
    Mail#mailData.text.

% @doc Replace tuples in `PropList' with `TupleList'. Returns new list.
-spec key_replace(list(), list()) -> list().
key_replace(TupleList, PropList) ->
    Fun = fun({K,V}, Acc) -> key_replace_1(lists:keyfind(K, 1, TupleList), K, V, Acc) end,
    lists:reverse(lists:foldl(Fun, [], PropList)).

% @private
key_replace_1(false, K, V, Acc) -> [{K,V} | Acc];
key_replace_1(Tuple, _K, _V, Acc) -> [Tuple | Acc].

% @doc Insert or replace tuples in `PropList' with `TupleList'. Returns new list.
-spec key_insert(list(), list()) -> list().
key_insert(TupleList, PropList) ->
    Fun = fun({K,V}, Acc) -> [{K,V} | lists:keydelete(K, 1, Acc)] end,
    lists:reverse(lists:foldl(Fun, PropList, TupleList)).

% @doc Returns `Option' value from app environment.
-spec get_opt(atom()) -> any().
get_opt(Option) ->
    {ok, Value} = application:get_env(Option),
    Value.

% @doc Removes leading/trailing spaces from `Text'.
-spec strip(binary()) -> binary().
strip(Text) ->
    re:replace(Text, "(^\\s+)|(\\s+$)", "", [global,{return,binary}]).

% @doc Replace the first `Limit' ocurrences of `SubStr' in `String' with `RepStr'.
-spec repstr(string(), string(), string(), pos_integer()) -> string().
repstr(String, SubStr, RepStr, Limit) ->
    repstr_1(String, SubStr, RepStr, Limit, "").

% @doc Replace every ocurrence of `SubStr' in `String' with `RepStr'.
-spec repstr(string(), string(), string()) -> string().
repstr(String, SubStr, RepStr) ->
    repstr_1(String, SubStr, RepStr, -1, "").

% @private
repstr_1(String, _SubStr, _RepStr, 0, Acc) ->
    string:concat(Acc, String);
repstr_1(String, SubStr, RepStr, Limit, Acc) ->
    Idx = string:str(String, SubStr),
    case Idx == 0 of
        true -> repstr_1(String, SubStr, RepStr, 0, Acc);
        false ->
            Left = string:concat(Acc, string:left(String, Idx - 1)),
            Right = string:right(String, string:len(String) - (Idx - 1) - string:len(SubStr)),
            repstr_1(Right, SubStr, RepStr, repstr_newlim(Limit), string:concat(Left, RepStr))
    end.

% @private
repstr_newlim(Limit) when Limit > 0 -> Limit - 1;
repstr_newlim(Limit) -> Limit.

% @doc Parse template.
-spec tpl_parse(binary(), binary(), binary()) -> tplInfo().
tpl_parse(Text, BegStr, EndStr) ->
    tpl_parse_1(open, Text, BegStr, EndStr, [], 0, size(Text)).

% @private
tpl_parse_1(open, Text, BegStr, EndStr, Acc, Start, Size) ->
    case binary:match(Text, BegStr, [{scope, {Start, Size - Start}}]) of
        nomatch ->
            #tpl{tagIndices=lists:reverse(Acc), text=Text, size=size(Text)};
        {Idx, Len} ->
            tpl_parse_1({close,Idx}, Text, BegStr, EndStr, Acc, Idx + Len, Size)
    end;
tpl_parse_1({close,OIdx}, Text, BegStr, EndStr, Acc, Start, Size) ->
    case binary:match(Text, EndStr, [{scope, {Start, Size - Start}}]) of
        nomatch ->
            #tpl{tagIndices=lists:reverse(Acc), text=Text, size=size(Text)};
        {Idx, Len} ->
            VarName = strip(binary_part(Text, {Start, Idx - Start})),
            CIdx = Idx + Len,
            tpl_parse_1(open, Text, BegStr, EndStr, [{VarName, OIdx, CIdx - OIdx}|Acc], CIdx, Size)
    end.

% @doc Replace template variables using `Dict'.
-spec tpl_render(tplInfo(), dict()) -> binary().
tpl_render(Info, Dict) ->
    tpl_render_1(Info#tpl.tagIndices, Info#tpl.text, 0, Info#tpl.size, Dict, []).

% @private
tpl_render_1([], Text, Start, Size, _Dict, Acc) ->
    iolist_to_binary([Acc, binary_part(Text, {Start, Size-Start})]);
tpl_render_1([{Name, Idx, Len}|Tags], Text, Start, Size, Dict, Acc) ->
    NextIdx = Idx + Len,
    tpl_render_1(Tags, Text, NextIdx, Size, Dict,
                 [Acc, binary_part(Text, {Start, Idx - Start}), dict:fetch(Name, Dict)]).
