-module(osn_sd).

-export([ready/0]).
-export([status/1]).

ready() ->
    set(<<"READY=1">>).

status(Token) ->
    set(<<"STATUS=TOKEN=", Token/binary>>).

set(Value) ->
    case os:getenv("NOTIFY_SOCKET") of
        false -> ok;
        Path ->
            {ok, Socket} = gen_udp:open(0, [local]),
            gen_udp:send(Socket, {local, Path}, 0, Value),
            gen_udp:close(Socket)
    end.
