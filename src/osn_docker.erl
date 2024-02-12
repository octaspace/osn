-module(osn_docker).

-export([apply/2]).

-define(TIMEOUT, timer:seconds(3600)).

apply(<<"docker/containers/ls">>, _Params) ->
    {ok, 200, Containers} = docker:g(<<"/containers/json">>, ?TIMEOUT),
    Containers;

apply(<<"docker/images">>, _Params) ->
    {ok, 200, Images} = docker:g(<<"/images/json">>, ?TIMEOUT),
    Images;

apply(<<"docker/pull">>, #{<<"image">> := Image}) ->
    case docker:p({<<"/images/create">>, [{<<"fromImage">>, Image}]}, #{}, ?TIMEOUT) of
        {ok, 200, _Message} -> #{};
        {ok, _Code, Message} ->
            {error, Message}
    end.
