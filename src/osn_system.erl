-module(osn_system).

-export([apply/2]).
-export([cpu_model_name/0]).

apply(<<"system">>, _Params) ->
    CPUUsage = cpu_usage(),
    #{
        erts             => osn:env(erts_version),
        uptime           => uptime(),
        version          => osn:version(),
        os_version       => osn:env(os_version),
        linux_distro     => osn:env(os_linux_distro),
        linux_release    => osn:env(os_linux_release),
        kernel_version   => osn:env(kernel_version),
        is_hive_os       => osn:env(is_hive_os),
        is_wsl           => osn:env(is_wsl),
        virt             => osn:env(virt),
        arch             => osn:env(system_arch),
        cpu              => CPUUsage,
        cpu_load_percent => cpu_load_percent(CPUUsage),
        cpu_model_name   => osn:env(cpu_model_name),
        gpu              => gpu_info(),
        cuda_version     => osn:env(cuda_version),
        memory           => memory_usage(),
        disk             => disk_usage()
    };

apply(<<"system/restart">>, _Params) ->
    erlang:halt(1);

apply(<<"system/upload">>, #{<<"filename">> := FileName, <<"data">> := Data} = Params) ->
    ok = file:write_file(FileName, base64:decode(Data)),
    case maps:get(<<"mode">>, Params, undefined) of
        undefined -> ok;
        Mode ->
            file:change_mode(FileName, Mode)
    end,
    #{}.

cpu_model_name() ->
    {ok, CPUInfo} = file:read_file("/proc/cpuinfo"),
    cpu_model_name(binary:split(CPUInfo, <<"\n">>, [global, trim_all])).

cpu_model_name([<<"model name\t: ", Model/binary>> | _Rest]) -> Model;
cpu_model_name([<<"Model\t\t: ", Model/binary>> | _Rest]) -> Model;
cpu_model_name([_Param | Rest]) -> cpu_model_name(Rest);
cpu_model_name([]) -> undefined.

uptime() ->
    element(1, erlang:statistics(wall_clock)) div 1000.

cpu_usage() ->
    Analyze =
        fun({N, UsageBy, Summary, _}, Acc) ->
            #{
                user   := User,
                kernel := Kernel
            } = maps:from_list(UsageBy),
            #{idle := Idle} = maps:from_list(Summary),
            [#{
                core   => N + 1,
                kernel => Kernel,
                user   => User,
                idle   => Idle
            } | Acc]
        end,
    lists:foldl(Analyze, [], osn_sysmon:cpu_usage()).

memory_usage() ->
    maps:from_list(memsup:get_system_memory_data()).

disk_usage() ->
    DockerFS = osn:env(docker_filesystem),
    DockerDriver = osn:env(docker_driver),
    DockerRootDir = binary_to_list(osn:env(docker_root_dir)),
    case lists:keyfind(DockerRootDir, 1, disksup:get_disk_data()) of
        {DockerRootDir, KBytes, Capacity} when DockerFS =:= <<"xfs">>, DockerDriver =:= <<"overlay2">> ->
            Size = KBytes * 1024,
            Used = Size div 100 * Capacity,
            #{
                size         => Size,
                used         => Used,
                free         => Size - Used,
                used_percent => Capacity
            };
        _ -> #{size => 0, used => 0, free => 0, used_percent => 0}
    end.

gpu_info() ->
    #{
        nvidia => (osn:env(gpu_info_nvidia))(),
        amd    => (osn:env(gpu_info_amd))()
    }.

cpu_load_percent(Usage) ->
    Total =
        lists:foldl(
            fun(#{idle := Idle}, Acc) -> Acc + Idle end,
            0,
            Usage
        ),
    100 - (Total / length(Usage)).
