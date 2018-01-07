-module(lic_table).

-export([
    new/1,
    new/2,
    delete/1
]).

-define(DEFAULT_MEMORY, erlang:memory(system)*0.3).

-define(DEFAULT_SIZE, no_limit).

-define(DEFAULT_OPTIONS, [
    {memory, ?DEFAULT_MEMORY},
    {size,   ?DEFAULT_SIZE}
]).

new(Name) ->
    new(Name, ?DEFAULT_OPTIONS).

new(Name, Options) ->
    case {is_options_valid(Options), is_exist(Name)} of
        {true, false} ->
            Tid = ets:new(time_table, [ordered_set, public]), 
            _   = ets:new(Name, [named_table, set, public]),
            ok = save_time_table(Name, Tid),
            ok = save_options(Name, Options),
            ok = lic_cleaner:start(Name);
        {false, _} ->
            {error, <<"invalid options">>};
        {_, true} ->
            {error, <<"alredy exist">>}
    end.

delete(Name) ->
    ok = lic_cleaner:stop(Name),
    _ = timer:sleep(10),
    case ets:lookup(lic_internal_info, {time_table, Name}) of
        [{_, Tid}] ->
            true = ets:delete(Tid);
        _ ->
            ok
    end,
    true = ets:delete(Name),
    true = ets:delete(lic_internal_info, {time_table, Name}),
    true = ets:delete(lic_internal_info, {options, Name, size}),
    true = ets:delete(lic_internal_info, {options, Name, memory}).

% internal

is_options_valid(Options) ->
    case (catch lists:unzip(Options)) of
        {Keys, Values} when (is_list(Keys) and is_list(Values)) ->
        KeysFun = fun(K) ->
            is_atom(K)
        end,
        ValsFun = fun(V) ->
            is_number(V) or is_atom(V)
        end,
        KeysValidate   = lists:all(KeysFun, Keys),
        ValuesValidate = lists:all(ValsFun, Values),
        (KeysValidate and ValuesValidate);
        _ ->
            false
    end.

is_exist(Name) ->
    All = ets:all(),
    lists:member(Name, All).

save_time_table(Name, Tid) ->
    ets:insert(lic_internal_info, {{time_table, Name}, Tid}),
    ok.

save_options(Name, Options) ->
    Size =   proplists:get_value(size, Options, ?DEFAULT_SIZE),
    Memory = proplists:get_value(memory, Options, ?DEFAULT_MEMORY),
    Objects = [
        {{options, Name, size},   Size},
        {{options, Name, memory}, Memory}
    ],
    ets:insert(lic_internal_info, Objects),
    ok.
