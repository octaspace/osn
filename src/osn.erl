-module(osn).

-export([version/0]).
-export([env/1]).
-export([env/2]).
-export([to_binary/1]).
-export([to_number/1]).

version() ->
    Version = lists:keyfind(orc, 1, application:which_applications()),
    list_to_binary(element(3, Version)).

env(Option) ->
    env(Option, undefined).

env(Option, Default) ->
    persistent_term:get({config, Option}, Default).

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_float(Value) -> float_to_binary(Value).

%% for some parameters like fan speed and power limit
%% nvidia-smi reported [N/A] value
to_number(<<"[N/A]">>) -> 0;

to_number(Value) when is_binary(Value) ->
    try
        binary_to_integer(Value)
    catch
        _:_Reason:_Stack -> %% probably value is float
            binary_to_float(Value)
    end.

%-export([set_sd_token/1]).

%set_sd_token(Token) ->
%    {ok, Socket} = gen_udp:open(0, [local]),
%    gen_udp:send(Socket, {local, os:getenv("NOTIFY_SOCKET")}, 0, <<"READY=1">>),
%    gen_udp:send(Socket, {local, os:getenv("NOTIFY_SOCKET")}, 0, <<"STATUS=TOKEN=", Token/binary>>),
%    gen_udp:close(Socket).
