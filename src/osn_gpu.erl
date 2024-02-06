-module(osn_gpu).

-export([info/1]).
-export([parse_output/2]).
-export([cuda_version/0]).

-include_lib("kernel/include/logger.hrl").

info(nvidia) ->
    Args = "--query-gpu=index,name,driver_version,pstate,pcie.link.gen.max,pcie.link.gen.current,temperature.gpu,utilization.gpu,utilization.memory,memory.total,memory.free,display_mode,display_active,fan.speed,power.limit --format=csv,nounits,noheader",
    gpu_info(lookup_nvidia_smi() ++ " " ++ Args, nvidia);

info(amd) -> gpu_info("./clinfo " ++ "--json", amd).

cuda_version() ->
    case osn_system_shell:exec("./cuda-version") of
        {0, Version} ->
            string:trim(Version);
        _ -> <<"0">>
    end.

gpu_info(Cmd, GPU) ->
    case osn_system_shell:exec(Cmd) of
        {0, Data} ->
            try
                parse_output(Data, GPU)
            catch
                _:Reason:Stack ->
                    ?LOG_ERROR("can't parse output: ~p, cmd: ~s, reason: ~p, stack: ~p", [
                        Data, Cmd, Reason, Stack
                    ]),
                    []
            end;
        Error ->
            ?LOG_ERROR("can't get GPU info, exec_path: ~s, error: ~p", [Cmd, Error]),
            []
    end.

parse_output(Data, nvidia) ->
    lists:foldl(
        fun(Info, Acc) ->
            [
                Idx,
                Model,
                DriverVersion,
                PState,
                PCIELinkMax,
                PCIELinkCurrent,
                TempGPU,
                UtilizationGPU,
                UtilizationMem,
                MemTotal,
                MemFree,
                DisplayMode,
                DisplayActive,
                FanSpeed,
                PowerLimit
            ] = binary:split(Info, <<", ">>, [trim_all, global]),
            [#{
                idx               => osn:to_number(Idx),
                model             => Model,
                driver_version    => DriverVersion,
                pstate            => PState,
                pcie_link_max     => osn:to_number(PCIELinkMax),
                pcie_link_current => osn:to_number(PCIELinkCurrent),
                gpu_temperature   => osn:to_number(TempGPU),
                gpu_utilization   => osn:to_number(UtilizationGPU),
                mem_utilization   => osn:to_number(UtilizationMem),
                mem_total_mb      => osn:to_number(MemTotal),
                mem_free_mb       => osn:to_number(MemFree),
                display_mode      => DisplayMode,
                display_active    => DisplayActive,
                fan_speed         => osn:to_number(FanSpeed),
                power_limit_watt  => osn:to_number(PowerLimit)
            } | Acc]
        end,
        [],
        binary:split(Data, <<"\n">>, [global, trim_all])
    );
parse_output(Data, amd) ->
    lists:flatten(lists:foldl(
        fun(#{<<"online">> := Online}, Acc) ->
            [process_amd_online_gpu(Online) | Acc];
           (_Info, Acc) -> Acc
        end,
        [],
        maps:get(<<"devices">>, jsx:decode(Data))
    )).

process_amd_online_gpu(GPUs) -> process_amd_online_gpu(GPUs, []).

process_amd_online_gpu([], Acc) -> Acc;
process_amd_online_gpu([#{<<"CL_DEVICE_VENDOR">> := <<"Advanced Micro Devices, Inc.">>} = Info | Rest], Acc) ->
    process_amd_online_gpu(Rest, [#{
        model        => maps:get(<<"CL_DEVICE_BOARD_NAME_AMD">>, Info),
        mem_total_mb => maps:get(<<"CL_DEVICE_GLOBAL_MEM_SIZE">>, Info) / 1024 / 1024 %% CL_DEVICE_GLOBAL_MEM_SIZE in bytes
    } | Acc]);
process_amd_online_gpu([#{<<"CL_DEVICE_VENDOR">> := _Vendor} | Rest], Acc) ->
    process_amd_online_gpu(Rest, Acc).

lookup_nvidia_smi() ->
    case osn:env(is_wsl) of
        true ->
            "/usr/lib/wsl/lib/nvidia-smi";
        false ->
            "/usr/bin/nvidia-smi"
    end.
