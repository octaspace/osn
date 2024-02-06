-module(osn_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_link_proc/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link_proc() ->
    supervisor:start_child(?MODULE, #{id => osn_link, start => {osn_link, start_link, []}, restart => transient}).

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.
