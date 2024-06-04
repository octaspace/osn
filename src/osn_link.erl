-module(osn_link).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_continue/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    gun_pid               :: undefined | pid(),
    gun_opts              :: map(),
    ws_stream             :: undefined | reference(),
    fcc = 0               :: pos_integer(),
    token                 :: binary(),
    connect_attempts = 10 :: pos_integer(),
    connect_timeout = 300 :: pos_integer()
}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts) ->
    PrivDir = code:priv_dir(osn),
    State = #state{
        token            = maps:get(<<"token">>, Opts),
        connect_attempts = maps:get(<<"attempts">>, Opts),
        connect_timeout  = maps:get(<<"timeout">>, Opts),
        gun_opts = #{
            protocols => [http],
            transport => tls,
            ws_opts => #{keepalive => 60000},
            tls_opts => [
                {certfile, filename:join([PrivDir, "osn-crt.pem"])},
                {keyfile, filename:join([PrivDir, "osn-key.pem"])},
                {verify, verify_none}
            ]
        }
    },
    {ok, State, {continue, establish_link}}.

handle_continue(establish_link, State) ->
    ?LOG_INFO("init link, auth attempts: ~p, timeout: ~p", [
        State#state.connect_attempts, State#state.connect_timeout
    ]),
    {ok, Pid} =
        case os:getenv("OSN_DEBUG") of
            false ->
                gun:open("osn.octa.computer", 30100, State#state.gun_opts);
            _ ->
                gun:open("localhost", 30100, State#state.gun_opts)
        end,
    monitor(process, Pid),
    {noreply, State#state{gun_pid = Pid}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Req, State) -> {noreply, State}.

handle_info({gun_up, _Pid, _Protocol}, State) ->
    ?LOG_INFO("link is up, upgrading protocol to websocket"),
    gun:ws_upgrade(State#state.gun_pid, "/", [{<<"authorization">>, State#state.token}]),
    {noreply, State};

handle_info({gun_upgrade, _Pid, StreamRef, _, _}, State) ->
    ?LOG_INFO("protocol upgraded successful"),
    {noreply, State#state{fcc = 0, ws_stream = StreamRef}};

handle_info({gun_ws, _Pid, _Ref, {text, Req}}, State) ->
    spawn(fun() -> handle_request(jsx:decode(Req), State) end),
    {noreply, State};

handle_info({gun_response, _Pid, _, _, Code, _Headers}, #state{fcc = FCC} = State) when Code =:= 401; Code =:= 404 ->
    ?LOG_ERROR("authentication failed, fcc: ~p", [State#state.fcc]),
    case FCC =:= State#state.connect_attempts of
        true ->
            ?LOG_INFO("max fcc is exceed, stop link process"),
            {stop, normal, State};
        false ->
            gun:close(State#state.gun_pid),
            timer:sleep(timer:seconds(State#state.connect_timeout)),
            {noreply, State#state{fcc = FCC + 1}}
    end;

handle_info({gun_down, _Pid, _Protocol, Reason, _Streams}, State) ->
    ?LOG_ERROR("link is down, reason: ~p", [Reason]),
    {noreply, State};

handle_info({gun_error, _Pid, _StreamRef, Reason}, State) ->
    ?LOG_ERROR("error on link, reason: ~p", [Reason]),
    {noreply, State};

handle_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    ?LOG_ERROR("link terminated, reason: ~p", [Reason]),
    {noreply, State, {continue, establish_link}};

handle_info(Msg, State) ->
    ?LOG_INFO("handle unexpected message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    gun:close(State#state.gun_pid).

send(#state{gun_pid = Pid, ws_stream = Stream} = _State, Req, Message) ->
    case maps:get(<<"pid">>, Req, undefined) of
        undefined -> %% cast
            ok;
        ReqPid ->
            gun:ws_send(Pid, Stream, {text, jsx:encode(Message#{pid => ReqPid})})
    end.

handle_request(#{<<"method">> := Method, <<"params">> := Params} = Req, State) ->
    ?LOG_DEBUG("handle request, method: ~p, params: ~p", [Method, Params]),

    try
        Reply =
            case erlang:apply(method_to_module(Method), apply, [Method, Params]) of
                {error, Reason} ->
                    #{<<"error">> => Reason};
                Result ->
                    #{<<"result">> => Result}
            end,
        send(State, Req, Reply)
    catch
        _:Reason0:_Stack ->
            Message = iolist_to_binary(io_lib:format("~p", [Reason0])),
            send(State, Req, #{<<"error">> => Message})
    end.

method_to_module(<<"system">>)                -> osn_system;
method_to_module(<<"system/restart">>)        -> osn_system;
method_to_module(<<"system/shell">>)          -> osn_system_shell;
method_to_module(<<"system/upgrade">>)        -> osn_system_upgrade;
method_to_module(<<"vrf/", _Rest/binary>>)    -> osn_vrf;
method_to_module(<<"docker/", _Rest/binary>>) -> osn_docker.
