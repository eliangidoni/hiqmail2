-module(rule).
-export([add/3,update/3]).

add(Sid, Env, Data) ->
    {ok, _Query} = decode(Sid, Env, Data),
    D1=hiqlib_rule:new_field_and(hiqlib_rule:new_field_number(<<"#type">>, "==", 10),
                                 hiqlib_rule:new_field_number(<<"#num">>, ">", 100)),
    D=hiqlib_rule:ruleset_fields(D1,hiqlib_rule:new_ruleset("1")),
    Result = send_to_server(add, Sid, D),
    send_response(Sid, Result).

update(Sid, Env, Data) ->
    {ok, _Query} = decode(Sid, Env, Data),
    D2=hiqlib_rule:new_field_and(hiqlib_rule:new_field_number(<<"#type">>, "==", 10),
                                 hiqlib_rule:new_field_number(<<"#num">>, ">", 90)),
    D3=hiqlib_rule:ruleset_fields(D2,hiqlib_rule:new_ruleset("1")),
    Result = send_to_server(update, Sid, D3),
    send_response(Sid, Result).

% @private
send_to_server(_, Sid, undefined) ->
    mod_esi:deliver(Sid, "Invalid parameters"),
    error;
send_to_server(add, _, Params) ->
    hiqcore:add_rule(Params);
send_to_server(update, _, Params) ->
    hiqcore:update_rule(Params).

% @private
get_request_data("GET", Data) ->
    httpd:parse_query(Data);
get_request_data("POST", Data) ->
    Data.

% @private
decode(Sid, Env, Request) ->
    case hiqlib:json_decode(get_request_data(proplists:get_value(request_method, Env), Request)) of
        {error, V} -> mod_esi:deliver(Sid, [V, io_lib:format("~p",[Request])]);
        Other -> Other
    end.

% @private
send_response(Sid, Response) ->
    {ok, Json} = hiqlib:json_encode(Response),
    mod_esi:deliver(Sid, ["Content-Type: application/json;charset=utf-8\r\n\r\n", Json]).
