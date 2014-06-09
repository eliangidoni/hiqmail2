-module(test).
-export([hello/3]).

hello(Sid, _Env, Data) ->
%    [_, Headers] = Env,
% ++ proplists:get_value(remote_addr,Headers)
    mod_esi:deliver(Sid, "Content-Type: application/json;charset=utf-8\r\n\r\n"),
    {ok, Json} = hiqlib:json_encode([list_to_binary(Data),httpd:parse_query(Data),<<"CHAN">>]),
    mod_esi:deliver(Sid, Json).
