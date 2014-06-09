% @doc
% Hiqcore Public API.
% Input validation *must* be done here.
% @end

-module(hiqcore).

-export([add_rule/1, update_rule/1, remove_rule/1, queue_mail/2]).

add_rule(Rule) ->
    hiqcore_store:insert_rule(Rule).

update_rule(Rule) ->
    hiqcore_store:update_rule(Rule).

remove_rule(Rule) ->
    hiqcore_store:delete_rule(Rule).

queue_mail(_Params, _Template) ->
    ok.
