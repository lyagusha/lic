-module(lic_table_worker).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).
    
%% gen_server callbacks
init([Name]) ->
    TimeTable = ets:new(time_table, [ordered_set, public]),
    _         = ets:new(Name, [named_table, set, public]),
    _ = ets:insert(lic_internal_info, {{time_table, Name}, TimeTable}),
    [{_, Size}]   = ets:lookup(lic_internal_info, {options, Name, row_count}),
    [{_, Memory}] = ets:lookup(lic_internal_info, {options, Name, memory}),
    State = #{
        name => Name,
        time_table => TimeTable,
        size => Size,
        memory => Memory
    },
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(clear, State) ->
    #{
         name := Name,    
         size := Size,
         memory := Memory
    } = State,
    _ = ets:update_element(lic_internal_info, {worker, Name}, {3, busy}),
    ok = row_count_cleaner(Name, Size, State),
    ok = memory_cleaner(Name, Memory, State),
    _ = ets:update_element(lic_internal_info, {worker, Name}, {3, ready}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal
row_count_cleaner(Name, Size, State) ->
    RealSize = ets:info(Name, size),
    SizeOvershoot = case Size of
        unlimited -> 0;
        S when is_integer(S) -> RealSize - S
    end,
    case SizeOvershoot > 0 of
        false ->
            ok;
        true  ->
            delete_oldest(Name, State, SizeOvershoot),
            ok
    end.

memory_cleaner(Name, Memory, State) ->
    RealMemory = ets:info(Name, memory),
    case RealMemory > Memory of
        false -> 
            ok;
        true  ->            
            ok = delete_oldest(Name, State),
            memory_cleaner(Name, Memory, State)
    end.

delete_oldest(_Name, _State, 0) ->
    ok;
delete_oldest(Name, State, SizeOvershoot) ->
    delete_oldest(Name, State),
    delete_oldest(Name, State, SizeOvershoot-1).

delete_oldest(Name, #{time_table := Tid}) ->
    KeyInTimeTab = ets:first(Tid),
    case ets:lookup(Tid, KeyInTimeTab) of
        [{_, Key}] -> lic_data:delete(Name, Key);
        _          -> ignore
    end,
    ok.
