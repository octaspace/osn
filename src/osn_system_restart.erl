-module(osn_system_restart).

-export([apply/1]).

apply(_Params) ->
    erlang:halt(1).
