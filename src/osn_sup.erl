-module(osn_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_link_proc/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_proc(Opts) ->
    supervisor:start_child(?MODULE, #{id => osn_link, start => {osn_link, start_link, [Opts]}, restart => transient}).

init([]) ->
    Childs = [
        %#{id => osn_vrf, start => {osn_vrf, start_link, []}},
        #{id => osn_sysmon, start => {osn_sysmon, start_link, []}}
    ],
    {ok, {{one_for_all, 0, 1}, Childs}}.
