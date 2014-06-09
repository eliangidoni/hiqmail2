% @doc
% Hiqlib Rule sets Implementation.
% @end

-module(hiqlib_rule).

-export([ruleset_fields/1, ruleset_id/1, ruleset_eval/2, ruleset_decode/2]).

-record(ruleSet, {id, fields={none,none,none,none}}).

-opaque ruleSet()::#ruleSet{}.
-opaque field()::any().

% @doc Return fields from `Ruleset'.
-spec ruleset_fields(ruleSet()) -> field().
ruleset_fields(Ruleset=#ruleSet{}) ->
    Ruleset#ruleSet.fields.

% @doc Return ruleset Id.
-spec ruleset_id(ruleSet()) -> string().
ruleset_id(Set=#ruleSet{}) ->
    Set#ruleSet.id.

% @doc
% ```
%       Type = string | number
% '''
% Evaluate very `Ruleset' field.
% @end
-spec ruleset_eval(ruleSet(), dict()) -> boolean().
ruleset_eval(Ruleset=#ruleSet{}, Dict) ->
    ruleset_eval_1(Ruleset#ruleSet.fields, Dict).

% @private
ruleset_eval_1({none,_,_,_}, _) ->
    false;
ruleset_eval_1({op, 'or', {Field1, Field2}}, Dict) ->
    ruleset_eval_1(Field1, Dict) orelse ruleset_eval_1(Field2, Dict);
ruleset_eval_1({op, 'and', {Field1, Field2}}, Dict) ->
    ruleset_eval_1(Field1, Dict) andalso ruleset_eval_1(Field2, Dict);
ruleset_eval_1({Type, Name, Operator, Value}, Dict) ->
    evaluate(Type, dict:find(Name, Dict), Operator, Value).

% @private
evaluate(_Type, error, _Operator, _Value) ->
    false;
evaluate(num, {ok, Value2}, Operator, Value) ->
    evaluate_operator(Operator, Value2, Value);
evaluate(str, {ok, Value2}, Operator, Value) ->
    evaluate_string_operator(Operator, Value2, Value).

% @private
evaluate_string_operator(contains, V1, V2) -> binary:match(V1,V2) /= nomatch;
evaluate_string_operator(prefix,   V1, V2) -> binary:longest_common_prefix([V1,V2]) > 0;
evaluate_string_operator(suffix,   V1, V2) -> binary:longest_common_suffix([V1,V2]) > 0.

% @private
evaluate_operator(eq, V1, V2) -> V1 == V2;
evaluate_operator(ne, V1, V2) -> V1 /= V2;
evaluate_operator(ge, V1, V2) -> V1 >= V2;
evaluate_operator(le, V1, V2) -> V1 =< V2;
evaluate_operator(gt, V1, V2) -> V1 > V2;
evaluate_operator(lt, V1, V2) -> V1 < V2.

% @doc Decodes a JSON encoded ruleset.
-spec ruleset_decode(binary(), binary()) -> {ok, ruleSet()} | {error, string()}.
ruleset_decode(Id, Encoded) ->
    ruleset_decode_1(json:decode(Encoded), Id).

%@private
ruleset_decode_1({ok, [H|T]}, Id) ->
    {ok, #ruleSet{id=Id, fields=ruleset_decode_2(H, T)}};
ruleset_decode_1(Other, _Id) ->
    Other.

%@private
ruleset_decode_2(<<"and">>, [E1, E2]) ->
    [H1 | T1] = E1,
    [H2 | T2] = E2,
    {op, op, 'and', ruleset_decode_2(H1, T1), ruleset_decode_2(H2, T2)};
ruleset_decode_2(<<"or">>, [E1, E2]) ->
    [H1 | T1] = E1,
    [H2 | T2] = E2,
    {op, op, 'or', ruleset_decode_2(H1, T1), ruleset_decode_2(H2, T2)};
ruleset_decode_2(<<"num">>, [Name, <<"==">>, Value]) ->
    {num, Name, eq, Value};
ruleset_decode_2(<<"num">>, [Name, <<"!=">>, Value]) ->
    {num, Name, ne, Value};
ruleset_decode_2(<<"num">>, [Name, <<">">>, Value]) ->
    {num, Name, gt, Value};
ruleset_decode_2(<<"num">>, [Name, <<"<">>, Value]) ->
    {num, Name, lt, Value};
ruleset_decode_2(<<"num">>, [Name, <<">=">>, Value]) ->
    {num, Name, ge, Value};
ruleset_decode_2(<<"num">>, [Name, <<"<=">>, Value]) ->
    {num, Name, le, Value};
ruleset_decode_2(<<"str">>, [Name, <<"contains">>, Value]) ->
    {str, Name, contains, Value};
ruleset_decode_2(<<"str">>, [Name, <<"prefix">>, Value]) ->
    {str, Name, prefix, Value};
ruleset_decode_2(<<"str">>, [Name, <<"suffix">>, Value]) ->
    {str, Name, suffix, Value}.
