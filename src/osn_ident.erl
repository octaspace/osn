-module(osn_ident).

-export([start/0]).
-export([fetch/0]).
-export([send_register_request/1]).

-define(IDENT_FILE, "osn.ident").

start() ->
    check_is_registered().

fetch() ->
    case file:read_file(?IDENT_FILE) of
        {ok, Data} ->
            binary_to_term(Data);
        _ -> #{}
    end.

write(Data) ->
    ok = file:write_file(?IDENT_FILE, term_to_binary(Data, [compressed]), [sync]).

check_is_registered() ->
    case fetch() of
        #{<<"is_registered">> := true} ->
            osn_sup:start_link_proc();
        _Data ->
            send_register_request(10)
    end.

send_register_request(0) -> ok;
send_register_request(N) ->
    PrivDir = code:priv_dir(osn),
    HTTPOpts = [
        {ssl, [
            {certfile, filename:join([PrivDir, "osn-crt.pem"])},
            {keyfile, filename:join([PrivDir, "osn-key.pem"])},
            {verify, verify_none}
        ]}
    ],
    case httpc:request(get, {register_url(), []}, HTTPOpts, []) of
        {ok, {{"HTTP/1.1", 200, _OK}, _Headers, Body}} ->
            #{<<"token">> := Token} = jsx:decode(list_to_binary(Body)),
            write(#{<<"is_registered">> => true, <<"token">> => Token}),
            osn_sd:status(Token),
            osn_sup:start_link_proc();
        _Error ->
            timer:apply_after(60000, ?MODULE, send_register_request, [N - 1])
    end.

register_url() ->
    case os:getenv("OSN_DEBUG") of
        false ->
            "https://osn.octa.computer/hello";
        _ ->
            os:getenv("OSN_REGISTER_URL", "https://localhost:30100/hello")
    end.
