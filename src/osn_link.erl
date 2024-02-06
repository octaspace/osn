-module(osn_link).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_continue/2]).
-export([handle_info/2]).
-export([terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(MAX_AUTH_FAILED, 10).
-define(RECONNECT_TIMEOUT, 2000).

-record(state, {
    gun_pid   :: undefined | pid(),
    gun_opts  :: map(),
    ws_stream :: undefined | reference(),
    fcc = 0   :: pos_integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    PrivDir = code:priv_dir(osn),
    State = #state{
        gun_opts = #{
            protocols => [http],
            transport => tls,
            ws_opts => #{keepalive => 5000},
            tls_opts => [
                {certfile, filename:join([PrivDir, "osn-crt.pem"])},
                {keyfile, filename:join([PrivDir, "osn-key.pem"])},
                {verify, verify_none}
            ]
        }
    },
    {ok, State, {continue, establish_link}}.

handle_continue(establish_link, State) ->
    ?LOG_INFO("init link"),
    %{ok, Pid} = gun:open("osn.octa.computer", 30100, State#state.gun_opts),
    {ok, Pid} = gun:open("192.168.1.84", 9990, State#state.gun_opts),
    monitor(process, Pid),
    {noreply, State#state{gun_pid = Pid}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) -> {noreply, State}.

handle_info({gun_up, _Pid, _Protocol}, State) ->
    ?LOG_INFO("link is up, upgrading protocol to websocket"),
    #{<<"token">> := Token} = osn_ident:fetch(),
    gun:ws_upgrade(State#state.gun_pid, "/", [{<<"authorization">>, Token}]),
    {noreply, State};

handle_info({gun_upgrade, _Pid, StreamRef, _, _}, State) ->
    ?LOG_INFO("protocol upgraded successful"),
    {noreply, State#state{fcc = 0, ws_stream = StreamRef}};

handle_info({gun_ws, _Pid, _Ref, {text, Req}}, State) ->
    spawn(fun() -> handle_request(jsx:decode(Req), State) end),
    {noreply, State};

handle_info({gun_response, _Pid, _, _, 401, _Headers}, #state{fcc = FCC} = State) ->
    ?LOG_ERROR("authentication failed, fcc: ~p", [State#state.fcc]),
    case FCC =:= ?MAX_AUTH_FAILED of
        true ->
            ?LOG_INFO("max fcc is exceed, stop link process"),
            {stop, normal, State};
        false ->
            gun:close(State#state.gun_pid),
            timer:sleep(?RECONNECT_TIMEOUT),
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

send(#state{gun_pid = Pid, ws_stream = Stream} = _State, Message) ->
    gun:ws_send(Pid, Stream, {text, jsx:encode(Message)}).

handle_request(#{<<"method">> := Method, <<"params">> := Params} = Req, State) ->
    ?LOG_DEBUG("handle request, method: ~p, params: ~p", [Method, Params]),

    Reply = erlang:apply(method_to_module(Method), apply, [Params]),


    %send(State, #{pid => maps:get(<<"pid">>, Req), result => Reply}).
    send(State, #{pid => maps:get(<<"pid">>, Req), result => Reply}).

method_to_module(<<"system/shell">>) -> osn_system_shell.
