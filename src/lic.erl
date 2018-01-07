-module(lic).

-export([
    init/0,
    table_new/1,
    table_new/2,
    table_delete/1,
    set/3,
    get/2,
    get/3,
    delete/2
]).

-define(NAME_ERROR, <<"table name is not an atom">>).

init() ->
    _ = ets:new(lic_internal_info, [named_table, public, set]),
    ok.

table_new(Name) when is_atom(Name)->
    lic_table:new(Name);
table_new(_) ->
    {error, ?NAME_ERROR}.

table_new(Name, Options) when is_atom(Name)->
    lic_table:new(Name, Options);
table_new(_, _) ->
    {error, ?NAME_ERROR}.

table_delete(Name) when is_atom(Name)->
    lic_table:delete(Name);
table_delete(_) ->
    {error, ?NAME_ERROR}.

set(Name, Key, Value) when is_atom(Name)->
    lic_data:set(Name, Key, Value);
set(_, _, _) ->
    {error, ?NAME_ERROR}.

get(Name, Key) when is_atom(Name)->
    lic_data:get(Name, Key);
get(_, _) ->
    {error, ?NAME_ERROR}.

get(Name, Key, Default) when is_atom(Name)->
    lic_data:get(Name, Key, Default);
get(_, _, _) ->
    {error, ?NAME_ERROR}.

delete(Name, Key) when is_atom(Name)->
    lic_data:delete(Name, Key);
delete(_, _) ->
    {error, ?NAME_ERROR}.
