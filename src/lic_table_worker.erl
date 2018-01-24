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
    ok = save_time_table(Name, TimeTable),
    [{_, Size}]   = ets:lookup(lic_internal_info, {options, Name, size}),
    [{_, Memory}] = ets:lookup(lic_internal_info, {options, Name, memory}),
    State = #{
        name => Name,
        size => Size,
        memory => Memory
    },
    self() ! check,
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check, State) ->
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
            _ = [delete_oldest(Name) || _ <- lists:seq(1, SizeOvershoot)]
    end,
    RealMemory = ets:info(Name, memory),
    case RealMemory > Memory of
        false -> 
            ok;
        true  ->
            
            ok = delete_oldest(Name)
    end,
    timer:sleep(10),
    self() ! check,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal
delete_oldest(Name) ->
    [{_, Tid}] = ets:lookup(lic_internal_info, {time_table, Name}),
    [{_, Key}] = ets:lookup(Tid, ets:first(Tid)),
    lic_data:delete(Name, Key).

save_time_table(Name, Tid) ->                                                    
     ets:insert(lic_internal_info, {{time_table, Name}, Tid}),                    
     ok. 
