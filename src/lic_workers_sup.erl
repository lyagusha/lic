-module(lic_workers_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_worker/1]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(TabName) ->
    supervisor:start_child(?MODULE, [TabName]).

init([]) ->
    TableWorker = {
        lic_table_worker,
        {lic_table_worker, start_link, []},
        transient, brutal_kill, worker, [lic_table_worker]
    },
    {ok, {{simple_one_for_one, 250, 5}, [TableWorker]}}.
