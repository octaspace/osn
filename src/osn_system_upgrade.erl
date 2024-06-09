-module(osn_system_upgrade).

-export([apply/2]).

-include_lib("kernel/include/logger.hrl").

apply(<<"system/upgrade">>, #{<<"v">> := Version} = _Params) ->
    case osn:version() =:= Version of
        true ->
            ?LOG_INFO("system upgrade, version ~s already installed", [Version]);
        false ->
            case download(release_url(Version)) of
                ok ->
                    timer:apply_after(5000, erlang, halt, [1]),
                    ok;
                _ -> {error, <<"can't download release">>}
            end
    end.

release_url(Version) ->
    iolist_to_binary([
        "https://github.com/octaspace/osn/releases/download/v",
        Version,
        "/osn-",
        Version,
        "-ubuntu-",
        osn:env(os_linux_release),
        "-",
        atom_to_binary(osn:env(system_arch)),
        ".tar.gz"
    ]).

download(URL) ->
    FileName = filename:join([osn:env(cwd), "upgrade.tar.gz"]),
    file:delete(FileName),
    ?LOG_INFO("system upgrade, download release: ~s", [URL]),
    case httpc:request(get, {URL, []}, [], [{sync, true}, {stream, FileName}]) of
        {ok, saved_to_file} ->
            ?LOG_INFO("system upgrade, download completed, saved: ~s", [FileName]);
        Error -> Error
    end.
