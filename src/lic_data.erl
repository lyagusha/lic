-module(lic_data).

-export([set/4]).
-export([get/2, get/3]).
-export([delete/2]).

-define(NAME_ERROR, table_name_is_not_an_atom).

set(Tab, Key, Value, TTL) when is_atom(Tab) ->
    TimeKey = case ets:lookup(Tab, Key) of
        [{_, _, TK, _}] -> TK;
        _               -> null
    end,
    Expiry = case is_integer(TTL) of
        true  ->
            erlang:system_time(seconds) + TTL;
        false ->
            TTL
    end,
    true = ets:insert(Tab, {Key, Value, null, Expiry}),
    ok = update_time(Tab, Key, TimeKey),
    clear(Tab),
    ok;
set(_, _, _, _) ->
    {error, ?NAME_ERROR}.

get(Tab, Key) when is_atom(Tab) ->
    case ets:lookup(Tab, Key) of
        [{_, Val, TimeKey, infinity}] ->
            ok = update_time(Tab, Key, TimeKey),
            {ok, Val};
        [{_, Val, TimeKey, Expiry}] ->
            case Expiry >= erlang:system_time(seconds) of
                true  ->
                    ok = update_time(Tab, Key, TimeKey),
                    {ok, Val};
                false ->
                    delete(Tab, Key),
                    {error, undefined}
            end;
        _ ->
            {error, undefined}
    end;
get(_, _) ->
    {error, ?NAME_ERROR}.

get(Tab, Key, Def) when is_atom(Tab) ->
    case ets:lookup(Tab, Key) of
        [{_, Val, TimeKey, infinity}] ->
            ok = update_time(Tab, Key, TimeKey),
            {ok, Val};
        [{_, Val, TimeKey, Expiry}] ->
            case Expiry >= erlang:system_time(seconds) of
                true  ->
                    ok = update_time(Tab, Key, TimeKey),
                    {ok, Val};
                false ->
                    delete(Tab, Key),
                    {ok, Def}
            end;
        _ ->
            {ok, Def}
    end;
get(_, _, _) ->
    {error, ?NAME_ERROR}.

delete(Tab, Key) when is_atom(Tab) ->
    case ets:lookup(Tab, Key) of
        [{_, _, TimeKey, _}] ->
            [{_, TimeTab}] = ets:lookup(lic_internal_info, {time_table, Tab}),
            true = ets:delete(TimeTab, TimeKey);
        _ ->
            ok
    end,
    true = ets:delete(Tab, Key),
    ok;
delete(_, _) ->
    {error, ?NAME_ERROR}.

% internal
update_time(Tab, Key, TimeKey) ->
    [{_, TimeTab}] = ets:lookup(lic_internal_info, {time_table, Tab}),
    true = ets:delete(TimeTab, TimeKey),
    MonId = erlang:unique_integer([monotonic]),
    _ = ets:update_element(Tab, Key, {3, MonId}),
    true = ets:insert(TimeTab, {MonId, Key}),
    ok.

clear(Tab) ->
    [{_, WorkerPid, WorkerState}] = ets:lookup(lic_internal_info, {worker, Tab}),
    case WorkerState of
        ready ->
            WorkerPid ! clear;
        _ ->
            ignore
    end,
    ok.
