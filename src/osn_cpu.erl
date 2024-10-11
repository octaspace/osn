-module(osn_cpu).

-export([detect/0]).

detect() ->
    {0, Output} = osn_system_shell:exec("lscpu -J"),
    #{<<"lscpu">> := Data} = jsx:decode(Output),
    {VendorId, Info} = vendor_id(Data),
    Model = 
        case get_field(<<"Model name:">>, Data) of
            undefined ->
                {ok, Value, _} = get_field(<<"Model name:">>, maps:get(<<"children">>, Info)),
                Value;
            {ok, Value, _} -> Value
        end,
    persistent_term:put({config, cpu_vendor_id}, VendorId),
    persistent_term:put({config, cpu_model_name}, Model).

get_field(Field, [#{<<"field">> := Field} = Entry | _Rest]) ->
    {ok, maps:get(<<"data">>, Entry), Entry};
get_field(Field, [_Entry | Rest]) ->
    get_field(Field, Rest);
get_field(_Field, []) -> undefined.

vendor_id(Data) ->
    case get_field(<<"Vendor ID:">>, Data) of
        undefined -> <<"Unknown">>;
        {ok, VendorId, Entry} ->
            {format_vendor_id(VendorId), Entry}
    end.

format_vendor_id(<<"GenuineIntel">>) -> <<"INTEL">>;
format_vendor_id(<<"AuthenticAMD">>) -> <<"AMD">>;
format_vendor_id(VendorId) -> VendorId.
