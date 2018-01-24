-module(lic_table_worker).

-behaviour(gen_server).

-export([
    start_link/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

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
    _ = ets:insert(lic_internal_info, {{worker_state, self()}, busy}),
    #{
         name := Name,    
         size := Size,
         memory := Memory
    } = State,
    RealSize = ets:info(Name, size),
    SizeOvershoot = case Size of
        no_limit -> 0;
        S when is_integer(S) -> RealSize - S
    end,
    case SizeOvershoot > 0 of
        false ->
            ok;
        true  ->
            _ = [delete_oldest(Name, State) || _ <- lists:seq(1, SizeOvershoot)]
    end,
    RealMemory = ets:info(Name, memory),
    case RealMemory > Memory of
        false -> 
            ok;
        true  ->
            
            ok = delete_oldest(Name, State)
    end,
    _ = ets:insert(lic_internal_info, {{worker_state, self()}, ready}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal
delete_oldest(Name, #{time_table := Tid}) ->
    [{_, Key}] = ets:lookup(Tid, ets:first(Tid)),
    lic_data:delete(Name, Key).
