-module(transaction).

-export([create/1, get/1, update/1, close/3, set_status/2]).

-include("../include/transaction.hrl").

%% ---- create ----
%%
%% ParamsMap :
%% #{type,params,user_id,cost,new_balance,status,create_date, close_date}
create(ParamsMap) -> transaction_db:create(ParamsMap).

%% ---- get ----

get(#{transaction_id := TrId, mode := cache}) ->
    %% Получить transaction из кэша mnesia
    CacheUserServiceRes =get_cache(#{transaction_id => TrId}),
    %% io:format("DEBUG>>> transaction:get() get_cache res=~p~n", [CacheUserServiceRes]),
    case CacheUserServiceRes of
	%% Если записи нет, получаем объект из базы и помещаем в кэш
	[] ->
	    case transaction_db:get(#{tr_id => TrId}) of
		{ok, Tr} ->
		    CreateDateTimestamp0 = datetime_to_timestamp(Tr#transaction.create_date),
		    FinTr = Tr#transaction{create_date=CreateDateTimestamp0},
		    insert_cache(FinTr),
		    {ok, FinTr};
		{error, not_found} ->
		    {error, not_found}
	    end;
	[CacheTr] when is_record(CacheTr, transaction) ->
	    {ok, CacheTr}
    end;

%%
%% ParamsMap :
%% #{tr_id}
get(ParamsMap) -> transaction_db:get(ParamsMap).


%% ---- update ----
%%
%% ParamsMap :


update(#{transaction_id := TrId, old_status := OldStatus, new_status := NewStatus, mode := cache}) ->
    %% Получить user_service из кэша mnesia
    CacheTrRes =get_cache(#{transaction_id => TrId}),
    %% io:format("DEBUG>>> transaction:update() get_cache res: ~p~n", [CacheTrRes]),
    
    case CacheTrRes of
	%% Если записи нет, получаем объект из базы и помещаем в кэш
	[] ->
	    case transaction_db:get(#{tr_id => TrId}) of
		{ok, Tr} ->
		    CreateDateTimestamp0 = datetime_to_timestamp(Tr#transaction.create_date),
		    FinTr = Tr#transaction{create_date=CreateDateTimestamp0},
		    insert_cache(FinTr),
		    R = update_cache(#{transaction_id => TrId, old_status => OldStatus, new_status => NewStatus}),
		    %% io:format("DEBUG>>> transaction:update() update_cache1 res: ~p~n", [R]),
		    R;
		{error, not_found} ->
		    {error, not_found}
	    end;
	[CacheTr] when is_record(CacheTr, transaction), CacheTr#transaction.status==OldStatus ->
	    R = update_cache(#{transaction_id => TrId, old_status => OldStatus, new_status => NewStatus}),
	    %% io:format("DEBUG>>> transaction:update() update_cache2 res: ~p~n", [R]),
	    R;
	[CacheTr] when is_record(CacheTr, transaction) ->
	    {error, {bad_old_status, CacheTr#transaction.status}}
    end;


update(#{transaction_id := TrId, params := Params}) ->
    ParamsStr = lists:flatten(io_lib:format("~p.", [Params])),
    transaction_db:update(#{transaction_id => TrId, params => ParamsStr});

update(ParamsMap) ->
    transaction_db:update(ParamsMap).

%% ---- close ----
%% TrId : int()
%% Status : int() 
close(TrId, NewBalance, Status) ->
    CloseDate = calendar:local_time(),
    transaction_db:close(#{tr_id => TrId, new_balance => NewBalance, status => Status, close_date => CloseDate}).


%% ---- set_status ----
%% TrId : int()
%% Status : int() 
set_status(TrId, Status) -> transaction_db:set_status(TrId, Status).





%% MNESIA API %%

insert_cache(Tr) ->
    %%io:format("DEBUG>>> transaction:insert_cache pid: ~p  us: ~p~n", [self(), UserService#user_service.user_service_id]),
    F = fun() -> mnesia:write(transaction_cache, Tr, write) end,
    mnesia:activity(transaction, F).


update_cache(#{transaction_id := TrId, old_status := OldStatus, new_status := NewStatus}) ->
    Pattern = #transaction{_ = '_', transaction_id = TrId},
    F = fun() -> 
 		case mnesia:match_object(transaction_cache, Pattern, write) of
		    [] -> {error, not_found};
 		    [Tr] when is_record(Tr, transaction),Tr#transaction.status==OldStatus->
			NewTr = Tr#transaction{status=NewStatus},
			mnesia:write(transaction_cache, NewTr, write),
			{ok, NewTr};
		    [Tr] when is_record(Tr, transaction) ->
			{error, {bad_old_status, Tr#transaction.status}}
 		end
	end,
    mnesia:activity(transaction, F).


get_cache(#{transaction_id := TrId}) ->
    F = fun() -> mnesia:read({transaction_cache, TrId})  end,
    mnesia:activity(transaction, F).






datetime_to_timestamp(Datetime) ->
    FEndTimestamp = case Datetime of
			undefined -> undefined;
			{datetime, {{_,_,_},{_,_,_}} = DT2} ->
			    calendar:datetime_to_gregorian_seconds(DT2)
		    end,
    FEndTimestamp.
