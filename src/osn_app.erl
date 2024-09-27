-module(osn_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_StartType, _StartArgs) ->
    case osn_sup:start_link() of
        {ok, _Pid} = Sup ->
            set_cwd(),
            init_logger(),
            osn_ident:start(),
            collect_docker_info(),
            set_common_config(),
            osn_gpu:setup(),
            osn_sd:ready(),
            Sup;
        Error -> Error
    end.

stop(_State) -> ok.

set_cwd() ->
    {ok, Cwd} = file:get_cwd(),
    persistent_term:put({config, cwd}, Cwd).

init_logger() ->
    Config = #{
        level => debug,
        config => #{
            file               => "log/osn.log",
            max_no_files       => 10,
            max_no_bytes       => 10485760,
            burst_limit_enable => false
        },
        formatter => {logger_formatter, #{template => [time, " ", mfa, ":", line, " ", msg, "\n"], single_line => true}}
    },
    case os:getenv("OSN_DEBUG") of
        false ->
            logger:remove_handler(default);
        _ ->
            DefaultConfig = #{
                level => debug,
                config => #{
                    burst_limit_enable => false
                },
                formatter => {logger_formatter, #{template => [time, " ", msg, "\n"]}}
            },
            logger:set_handler_config(default, DefaultConfig)
    end,
    logger:set_primary_config(#{level => debug}),
    logger:add_handler(osn, logger_disk_log_h, Config).

collect_docker_info() ->
    {ok, 200, Info} = docker:g(<<"/info">>),
    case maps:get(<<"DriverStatus">>, Info) of
        null -> ok;
        DriverStatus ->
            lists:foreach(
                fun
                    ([<<"Backing Filesystem">>, Value]) ->
                        persistent_term:put({config, docker_filesystem}, Value);
                    ([_Opt, _Value]) -> ok
                end,
                DriverStatus
            )
    end,
    persistent_term:put({config, docker_driver}, maps:get(<<"Driver">>, Info)),
    persistent_term:put({config, docker_root_dir}, maps:get(<<"DockerRootDir">>, Info)),
    persistent_term:put({config, kernel_version}, maps:get(<<"KernelVersion">>, Info)),
    persistent_term:put({config, os_version}, maps:get(<<"OperatingSystem">>, Info)).

set_common_config() ->
    Arch =
        case erlang:system_info(system_architecture) of
            "x86_64-pc-linux-gnu"       -> x86_64;
            "aarch64-unknown-linux-gnu" -> aarch64;
            _                           -> unknown
    end,
    detect_virt(),
    detect_linux_distro(),
    persistent_term:put({config, system_arch}, Arch),
    persistent_term:put({config, erts_version}, list_to_binary(erlang:system_info(version))),
    persistent_term:put({config, cpu_model_name}, osn_system:cpu_model_name()),
    persistent_term:put({config, memory_spec}, osn_system:memory_spec()),
    persistent_term:put({config, cuda_version}, osn_gpu:cuda_version()).

detect_linux_distro() ->
    {0, Data} = osn_system_shell:exec("lsb_release -sci"),
    [Distro, Release] = binary:split(Data, <<"\n">>, [global, trim_all]),
    persistent_term:put({config, os_linux_distro}, string:lowercase(Distro)),
    persistent_term:put({config, os_linux_release}, Release),
        
    {0, Kernel} = osn_system_shell:exec("uname -r"),
    NewKernel = string:trim(Kernel),
            
    persistent_term:put({config, is_wsl}, binary:part(NewKernel, {byte_size(NewKernel), -4}) =:= <<"WSL2">>),
    persistent_term:put(
        {config, is_hive_os},  
        (binary:part(NewKernel, {byte_size(NewKernel), -6}) =:= <<"hiveos">> orelse filelib:is_regular("/etc/hiveos-release"))
    ).

detect_virt() ->
    {_, Virt} = osn_system_shell:exec("systemd-detect-virt"),
    persistent_term:put({config, virt}, string:trim(Virt)).
