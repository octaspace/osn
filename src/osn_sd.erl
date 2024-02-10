-module(osn_sd).

-export([ready/0]).
-export([set_token/1]).
-export([set_status/1]).

ready() ->
    set(<<"READY=1">>).

set_token(Token) ->
    set_status(<<"TOKEN=", Token/binary>>).

set_status(Status) ->
    set(<<"STATUS=", Status/binary>>).

set(Value) ->
    case os:getenv("NOTIFY_SOCKET") of
        false -> ok;
        Path ->
            {ok, Socket} = gen_udp:open(0, [local]),
            gen_udp:send(Socket, {local, Path}, 0, Value),
            gen_udp:close(Socket)
    end.
