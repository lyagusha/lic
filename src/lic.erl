-module(lic).

-export([new/1, new/2]).
-export([set/3, set/4]).
-export([get/2, get/3]).
-export([delete/2]).
-export([keys/1]).

new(Tab) ->
    lic_table:new(Tab, []).

new(Tab, Options) ->
    lic_table:new(Tab, Options).

keys(Tab) ->
    lic_table:keys(Tab). 

set(Tab, Key, Value) ->
    lic_data:set(Tab, Key, Value, infinity).

set(Tab, Key, Value, TTL) ->
    lic_data:set(Tab, Key, Value, TTL).

get(Tab, Key) ->
    lic_data:get(Tab, Key).

get(Tab, Key, Default) ->
    lic_data:get(Tab, Key, Default).

delete(Tab, Key) ->
    lic_data:delete(Tab, Key).
