-module(lic_table).

-export([
    new/2
]).

-define(NAME_ERROR, table_name_is_not_an_atom).
-define(DEFAULT_MEMORY, erlang:memory(system)*0.3).
-define(DEFAULT_SIZE, no_limit).

new(Name, Options) when is_atom(Name) ->
    case is_options_valid(Options) of
        true  ->
            ok = save_options(Name, Options),
            Resp = lic_workers_sup:start_worker(Name),
            case Resp of
                {ok, _}    -> ok;
                {error, _} -> {error, already_exists}
            end;
        false ->
            {error, invalid_options}
    end;
new(_, _) ->
    {error, ?NAME_ERROR}.

% internal
is_options_valid(Options) -> % TODO
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

save_options(Name, Options) ->
    Size =   proplists:get_value(row_count, Options, ?DEFAULT_SIZE),
    Memory = proplists:get_value(memory, Options, ?DEFAULT_MEMORY),
    Objects = [
        {{options, Name, row_count},   Size},
        {{options, Name, memory}, Memory}
    ],
    ets:insert(lic_internal_info, Objects),
    ok.
