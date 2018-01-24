-module(lic_app).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    _ = ets:new(lic_internal_info, [named_table, public, set]),
    _ = ets:new(lic_workers, [named_table, public, set]),
    lic_sup:start_link().

stop(_State) ->
    ok.
