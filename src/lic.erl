-module(lic).

-export([
    new/1,
    new/2,
    set/3,
    get/2,
    get/3,
    delete/2
]).

-define(NAME_ERROR, table_name_is_not_an_atom).

new(Name) when is_atom(Name)->
    lic_table:new(Name);
new(_) ->
    {error, ?NAME_ERROR}.

new(Name, Options) when is_atom(Name)->
    lic_table:new(Name, Options);
new(_, _) ->
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
