-module(osn_system).

-export([cpu_model_name/0]).

cpu_model_name() ->
    {ok, CPUInfo} = file:read_file("/proc/cpuinfo"),
    cpu_model_name(binary:split(CPUInfo, <<"\n">>, [global, trim_all])).

cpu_model_name([<<"model name\t: ", Model/binary>> | _Rest]) -> Model;
cpu_model_name([<<"Model\t\t: ", Model/binary>> | _Rest]) -> Model;
cpu_model_name([_Param | Rest]) -> cpu_model_name(Rest);
cpu_model_name([]) -> undefined.
