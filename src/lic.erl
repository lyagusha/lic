-module(lic).

-export([
    new/1,
    new/2,
    set/3,
    set/4,
    get/2,
    get/3,
    delete/2
]).

new(Name) ->
    lic_table:new(Name, []).

new(Name, Options) ->
    lic_table:new(Name, Options).

set(Name, Key, Value) ->
    lic_data:set(Name, Key, Value, infinity).

set(Name, Key, Value, TTL) ->
    lic_data:set(Name, Key, Value, TTL).

get(Name, Key) ->
    lic_data:get(Name, Key).

get(Name, Key, Default) ->
    lic_data:get(Name, Key, Default).

delete(Name, Key) ->
    lic_data:delete(Name, Key).
