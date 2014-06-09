-module(mail).
-export([send/3]).

send(Sid, Env, Data) ->
    {ok, Query} = decode(Sid, Env, Data),
    Result = send_to_server(Sid, proplists:get_value(<<"params">>, Query), proplists:get_value(<<"template">>, Query)),
    send_response(Sid, Result).

% @private
send_to_server(Sid, undefined, _) ->
    mod_esi:deliver(Sid, "Invalid parameters"),
    error;
send_to_server(Sid, _, undefined) ->
    mod_esi:deliver(Sid, "Invalid template"),
    error;
send_to_server(_, Params, Template) ->
    hiqcore:queue_mail(Params, Template).

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
