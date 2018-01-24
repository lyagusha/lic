How to use it

1. start app:
```
    application:start(lic).
```
2. init your table:
```
	lic:new(tab_name, [{memory, 10000000}, {size, 100000}]) -> ok.
```
    or
```
    lic:new(tab_name) -> ok.
```
3. insert data:
```
	lic:set(tab_name, Key, Val) -> ok.
```
4. get your data:
```
	lic:get(tab_name, Key) -> {ok, Value} | {error, undefined}.
```
    or
```
    lic:get(tab_name, Key, Def) -> {ok, Value} | {ok, Def}.
```
5. you can delete your data:
```
	lic:delete(tab_name, Key) -> ok.
```