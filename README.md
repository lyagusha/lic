### How to use it

1. start app:  
`application:start(lic).`  
2. init your table:  
`lic:new(TabName, Options) -> ok.`  
or  
`lic:new(TabName) -> ok.`  
3. insert data:  
`lic:set(TabName, Key, Val) -> ok.`  
or  
`lic:set(TabName, Key, Val, TTL) -> ok.`  
4. get your data:  
`lic:get(TabName, Key) -> {ok, Value} | {error, undefined}.`  
or  
`lic:get(TabName, Key, Def) -> {ok, Value} | {ok, Def}.`  
5. you can delete your data:  
`lic:delete(TabName, Key) -> ok.`  

### Types:
```
TabName = atom(),  
Options = [Option],  
Option  = {memory, Mem} | {row_count, Count}    
Mem   = integer(), memory in bytes,  
Count = integer(),  
Key   = term(),  
Val   = term(),  
Def   = term(),  
TTL   = integer(), time in seconds.  
```