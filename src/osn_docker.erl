-module(osn_docker).

-feature(maybe_expr, enable).
-compile({no_auto_import, [apply/2]}).

-export([apply/2]).

-define(TIMEOUT, timer:seconds(3600)).

apply(<<"docker/containers/ls">>, _Params) ->
    {ok, 200, Containers} = docker:g(<<"/containers/json">>, ?TIMEOUT),
    Containers;

apply(<<"docker/images">>, _Params) ->
    {ok, 200, Images} = docker:g(<<"/images/json">>, ?TIMEOUT),
    Images;

apply(<<"docker/pull">>, #{<<"Image">> := Image} = _Params) ->
    Images = apply(<<"docker/images">>, #{}),
    case lists:any(fun(#{<<"RepoTags">> := RT}) -> RT =/= null andalso lists:member(Image, RT) end, Images) of
        true -> #{};
        false ->
            case docker:p({<<"/images/create">>, [{<<"fromImage">>, Image}]}, #{}, ?TIMEOUT) of
                {ok, 200, _Message} -> #{};
                {ok, _Code, Message} ->
                   {error, Message}
            end
    end;

apply(<<"docker/logs">>, #{<<"Name">> := Name} = Params) ->
    Opts = [
        {<<"stdout">>, <<"true">>},
        {<<"stderr">>, <<"true">>},
        {<<"timestamps">>, <<"true">>},
        {<<"follow">>, <<"false">>},
        {<<"tail">>, maps:get(<<"tail">>, Params, <<"all">>)}
    ],
    case docker:g({<<"/containers/", Name/binary, "/logs">>, Opts}, ?TIMEOUT) of
        {ok, 200, Logs} ->
            base64:encode(zlib:gzip(Logs));
        {ok, 404, Message} ->
            {error, Message}
    end;

apply(<<"docker/stop">>, #{<<"Name">> := Name} = _Params) ->
    case docker:p(<<"/containers/", Name/binary, "/stop">>, #{}, ?TIMEOUT) of
        {ok, Code, _Message} when Code =:= 204; Code =:= 304 ->
            #{};
        {ok, _Code, Message} ->
            {error, Message}
    end;

apply(<<"docker/remove">>, #{<<"Name">> := Name} = _Params) ->
    docker:d(<<"/containers/", Name/binary>>, ?TIMEOUT),
    #{};

apply(<<"docker/kill">>, #{<<"Name">> := Name} = _Params) ->
    docker:p(<<"/containers/", Name/binary, "/kill">>, #{}, ?TIMEOUT),
    #{};

apply(<<"docker/stats">>, #{<<"Name">> := Name} = _Params) ->
    Opts = [{<<"stream">>, <<"false">>}, {<<"one-shot">>, <<"true">>}],
    case docker:g({<<"/containers/", Name/binary, "/stats">>, Opts}, ?TIMEOUT) of
        {ok, 200, Message} ->
            Message;
        {ok, _Code, Message} ->
            {error, Message}
    end;

apply(<<"docker/run">>, #{<<"Name">> := Name} = Params) ->
    maybe
        #{} ?= apply(<<"docker/pull">>, Params),
        {ok, 201, Data} ?= docker:p({<<"/containers/create">>, [{<<"name">>, Name}]}, Params, ?TIMEOUT),
        {ok, 204, _} ?= docker:p(<<"/containers/", Name/binary, "/start">>, #{}, ?TIMEOUT),
        Data
    else
        {ok, _Code, Error} -> {error, Error};
        Error -> Error
    end.
