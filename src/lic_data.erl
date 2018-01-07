-module(lic_data).

-export([
    set/3,
    get/2,
    get/3,
    delete/2
]).

-define(PRINT(V), io:format("~n~p~n~p~n", [{?MODULE,?LINE},V])).

set(Tab, Key, Value) ->
    Now = erlang:system_time(),
    true = ets:insert(Tab, {Key, Value, Now}),
    [{_, TimeTab}] = ets:lookup(lic_internal_info, {time_table, Tab}),
    true = ets:insert(TimeTab, {Now, Key}),
    ok.

get(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_, Val, TimeKey}] ->
            ok = update_time(Tab, Key, TimeKey),
            {ok, Val};
        _ ->
            {error, undefined}
    end.

get(Tab, Key, Def) ->
    case ets:lookup(Tab, Key) of
        [{_, Val, _}] ->
            {ok, Val};
        _ ->
            {ok, Def}
    end.

delete(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [{_, _, TimeKey}] ->
            [{_, TimeTab}] = ets:lookup(lic_internal_info, {time_table, Tab}),
            true = ets:delete(TimeTab, TimeKey);
        _ ->
            ok
    end,
    true = ets:delete(Tab, Key),
    ok.

% internal

update_time(Tab, Key, TimeKey) ->
    Fun = fun() ->
        [{_, TimeTab}] = ets:lookup(lic_internal_info, {time_table, Tab}),
        true = ets:delete(TimeTab, TimeKey),
        Now = erlang:system_time(),
        _ = ets:update_element(Tab, Key, {3, Now}),
        true = ets:insert(TimeTab, {Now, Key})
    end,
    _ = spawn(Fun),
    ok.
