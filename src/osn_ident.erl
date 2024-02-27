-module(osn_ident).

-export([start/0]).
-export([fetch/0]).
-export([send_register_request/1]).

-include_lib("kernel/include/logger.hrl").

-define(IDENT_FILE, "osn.ident").

start() ->
    check_is_registered().

fetch() ->
    case file:read_file(?IDENT_FILE) of
        {ok, Data} ->
            binary_to_term(Data);
        _ -> undefined
    end.

write(Data) ->
    ok = file:write_file(?IDENT_FILE, term_to_binary(Data, [compressed]), [sync]).

check_is_registered() ->
    case fetch() of
        undefined ->
            osn_sd:set_status(<<"Request node registration token...">>),
            send_register_request(10);
        Data ->
            osn_sup:start_link_proc(Data)
    end.

send_register_request(0) ->
    osn_sd:set_status(<<"Can't request node token">>);
send_register_request(N) ->
    PrivDir = code:priv_dir(osn),
    HTTPOpts = [
        {ssl, [
            {certfile, filename:join([PrivDir, "osn-crt.pem"])},
            {keyfile, filename:join([PrivDir, "osn-key.pem"])},
            {verify, verify_none}
        ]}
    ],
    ?LOG_INFO("attempt request node token, n: ~p", [N]),
    case httpc:request(get, {register_url(), []}, HTTPOpts, []) of
        {ok, {{"HTTP/1.1", 200, _OK}, _Headers, Body}} ->
            #{<<"token">> := Token} = Data = jsx:decode(list_to_binary(Body)),
            write(Data),
            osn_sd:set_token(Token),
            osn_sup:start_link_proc(Data);
        _Error ->
            timer:apply_after(60000, ?MODULE, send_register_request, [N - 1])
    end.

register_url() ->
    case os:getenv("OSN_DEBUG") of
        false ->
            "https://osn.octa.computer:30100/hello";
        _ ->
            os:getenv("OSN_REGISTER_URL", "https://localhost:30100/hello")
    end.
