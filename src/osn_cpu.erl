-module(osn_cpu).

-export([detect/0]).
-export([fallback_detect/0]).

detect() ->
    {0, Output} = osn_system_shell:exec("lscpu -J"),
    #{<<"lscpu">> := Data} = jsx:decode(Output),
    case vendor_id(Data) of
        undefined ->
            fallback_detect();
        {VendorId, Info} ->
            Model = case get_field(<<"Model name:">>, Data) of
                undefined ->
                    {ok, Value, _} = get_field(<<"Model name:">>, maps:get(<<"children">>, Info)),
                    Value;
                {ok, Value, _} -> Value
            end,
        persistent_term:put({config, cpu_vendor_id}, VendorId),
        persistent_term:put({config, cpu_model_name}, Model)
    end.

get_field(Field, [#{<<"field">> := Field} = Entry | _Rest]) ->
    {ok, maps:get(<<"data">>, Entry), Entry};
get_field(Field, [_Entry | Rest]) ->
    get_field(Field, Rest);
get_field(_Field, []) -> undefined.

vendor_id(Data) ->
    case get_field(<<"Vendor ID:">>, Data) of
        undefined -> undefined;
        {ok, VendorId, Entry} ->
            {format_vendor_id(VendorId), Entry}
    end.

format_vendor_id(<<"GenuineIntel">>) -> <<"INTEL">>;
format_vendor_id(<<"AuthenticAMD">>) -> <<"AMD">>;
format_vendor_id(VendorId) -> VendorId.

fallback_detect() ->
     {ok, CPUInfo} = file:read_file("/proc/cpuinfo"),
     Data = binary:split(CPUInfo, <<"\n">>, [global, trim_all]),
     persistent_term:put({config, cpu_vendor_id}, fallback_vendor_id(Data)),
     persistent_term:put({config, cpu_model_name}, fallback_model_name(Data)).

fallback_vendor_id([<<"vendor_id\t: ", VendorId/binary>> | _Rest]) -> format_vendor_id(VendorId);
fallback_vendor_id([_Param | Rest]) -> fallback_vendor_id(Rest);
fallback_vendor_id([]) -> undefined.

fallback_model_name([<<"model name\t: ", Model/binary>> | _Rest]) -> Model;
fallback_model_name([<<"Model\t\t: ", Model/binary>> | _Rest]) -> Model;
fallback_model_name([_Param | Rest]) -> fallback_model_name(Rest);
fallback_model_name([]) -> undefined.
