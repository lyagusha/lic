-module(lic_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    WorkersSup = {
        lic_workers_sup,
        {lic_workers_sup, start_link, []},
		transient, infinity, supervisor, [lic_workers_sup]
    },
    {ok, {{one_for_one, 100, 1}, [WorkersSup]}}.
