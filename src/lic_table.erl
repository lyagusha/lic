-module(lic_table).

-export([new/2]).
-export([keys/1]).

-define(NAME_ERROR, table_name_is_not_an_atom).
-define(NOT_EXIST_ERROR, table_does_not_exist).

-define(DEFAULT_MEMORY, unlimited).
-define(DEFAULT_SIZE,   unlimited).

new(Tab, Options) when is_atom(Tab) ->
    case is_options_valid(Options) of
        true  ->
            ok = save_options(Tab, Options),
            Resp = lic_workers_sup:start_worker(Tab),
            case Resp of
                {ok, _}    -> ok;
                {error, _} -> {error, already_exists}
            end;
        false ->
            {error, invalid_options}
    end;
new(_, _) ->
    {error, ?NAME_ERROR}.

keys(Tab) when is_atom(Tab) ->
    case is_exist(Tab) of 
        true  ->
            First = ets:first(Tab),
            keys(Tab, First, []);
        false ->
            {error, ?NOT_EXIST_ERROR}
    end.

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

save_options(Tab, Options) ->
    Size =   proplists:get_value(row_count, Options, ?DEFAULT_SIZE),
    Memory = proplists:get_value(memory, Options, ?DEFAULT_MEMORY),
    Objects = [
        {{options, Tab, row_count},   Size},
        {{options, Tab, memory}, Memory}
    ],
    ets:insert(lic_internal_info, Objects),
    ok.

keys(_Tab, '$end_of_table', Res) ->
    {ok, Res};
keys(Tab, Key, Res) ->
    Next = ets:next(Tab, Key),
    keys(Tab, Next, [Key|Res]).
   
is_exist(Tab) ->
    case ets:info(Tab) of
        [_|_]     -> true;
        undefined -> false
    end.
