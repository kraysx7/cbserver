-module(user_promocode).

-export([create/1, get/1, update/1]).

-include_lib("stdlib/include/qlc.hrl").

-include("include/user_promocode.hrl").

%% ---- create ----
%%
%% ParamsMap : #{...}
%%
create(#{promocode_id := PromocodeId,
	 user_id := UserId,
	 params := _Params,
	 activate_date_ts := _CurDateTs,
	 activate_date_dt := _CurDateDt,
	 activate_ip := _ActivateIpInt,
	 activate_ip_alpha := _ActivateIpAlpha,
	 status := _Status,
	 
	 activate_date_from_ts := _ActivateDateFromTs,
	 activate_date_from_dt := ActivateDateFromDt,
	 mode := cache} = ParamsMap) -> 
    %% Записываем пользовательский промокод пока без uid в mnesia кэш 
    case prepare_insert_cache(ParamsMap) of
	{new, PrepareUserPromocode} ->
	    %% Псевдо промокода не было в кэше...пробуем получить из базы
	    case user_promocode_db:get(#{promocode_id => PromocodeId, user_id => UserId, activate_date_from => ActivateDateFromDt}) of
		{ok, UserPromocode} ->
		    %% А в базе что то было..значит был сброс кэша

		    %% Теперь надо только произвести замену 'псевдо' в кэше
		    OldUserPromocodeId = PrepareUserPromocode#user_promocode.user_promocode_id,

		    NewUserPromocodeId = UserPromocode#user_promocode.user_promocode_id,
		    CacheActivateDateTs = commons:datetime_to_utc(UserPromocode#user_promocode.activate_date),
		    NewCacheUserPromocode = UserPromocode#user_promocode{user_promocode_id=NewUserPromocodeId, activate_date=CacheActivateDateTs},

		    replace_cache(#{old_user_promocode_id => OldUserPromocodeId, user_promocode => UserPromocode}),

		    %% и вернуть его 'как есть'
		    {ok, {old, NewCacheUserPromocode}};
		{error, not_found} ->
		    %% В базе тоже ничего не было, значит создаём
		    {ok, NewUserPromocode} = user_promocode_db:create(ParamsMap),

		    OldUserPromocodeId = PrepareUserPromocode#user_promocode.user_promocode_id,
		    NewUserPromocodeId = NewUserPromocode#user_promocode.user_promocode_id,
		    NewCacheUserPromocode = PrepareUserPromocode#user_promocode{user_promocode_id=NewUserPromocodeId},
		    replace_cache(#{old_user_promocode_id => OldUserPromocodeId, user_promocode => NewCacheUserPromocode}),
		    {ok, {new, NewCacheUserPromocode}}
	    end;
	{old, CacheUserPromocode} ->
	    %% Псевдо промокод уже был в кэше...кликер или нажатие несколько раз быстро
	    %% В этом случае возвращаем запись 'как есть' из кэша
	    %% user_promocode_db:create(ParamsMap)
	    {ok, {old, CacheUserPromocode}}
    end.


%% ---- get ----
%%
%% ParamsMap : #{...}
%%
get(ParamsMap) -> user_promocode_db:get(ParamsMap).


%% ---- update ----
%%
%% ParamsMap : #{...}
%%
update(ParamsMap) -> user_promocode_db:update(ParamsMap).




%% MNESIA API %%

prepare_insert_cache(#{promocode_id := PromocodeId,
		       user_id := UserId,
		       params := Params,
		       activate_date_ts := ActDateTs,
		       activate_date_dt := _ActDateDt,
		       activate_ip := ActivateIpInt,
		       activate_ip_alpha := ActivateIpAlpha,
		       status := Status,
		       
		       activate_date_from_ts := ActDateFromTs,
		       mode := cache}) ->
    F = fun() -> 
		QH_PC = qlc:q([PC || PC <- mnesia:table(user_promocode_cache),
				     PC#user_promocode.promocode_id == PromocodeId,
				     PC#user_promocode.user_id == UserId,
				     PC#user_promocode.activate_date > ActDateFromTs
			      ]),
		
		QlcRes = qlc:e(QH_PC),
		
		case QlcRes of
		    [] ->
			%% В кэше пусто..создаём 'псевдо' запись в кэше и возвращаем её
			%% в один момент времени пользователь может активировать только 1 промокод, поэтому псевдо id активируемого кода = offset+id пользователя
			PseudoUserPromocodeId = 10000000 + UserId,
			NewUserPromocode = #user_promocode{user_promocode_id=PseudoUserPromocodeId,
							   promocode_id=PromocodeId,
							   user_id=UserId,
							   params=Params,
							   activate_date=ActDateTs,
							   activate_ip=ActivateIpInt,
							   activate_ip_alpha=ActivateIpAlpha,
							   status=Status},
			
			io:format("DEBUG>>> user_promocode:prepare_insert_cache NEW UserPromocode: ~p~n", [NewUserPromocode]),
			
			mnesia:write(user_promocode_cache, NewUserPromocode, write),
			{new, NewUserPromocode};
		    [UserPromocode] when is_record(UserPromocode, user_promocode) ->
			io:format("DEBUG>>> user_promocode:prepare_insert_cache OLD UserPromocode: ~p~n", [UserPromocode]),
			
			%% Уже есть активация такого промокода в кэшэ..возвращаем что есть..
			{old, UserPromocode}
		end	    
	end,
    mnesia:activity(transaction, F).



replace_cache(#{old_user_promocode_id := OldUserPromocodeId, user_promocode := UserPromocode}) ->
    F = fun() -> 
		mnesia:delete(user_promocode_cache, OldUserPromocodeId, write),
		mnesia:write(user_promocode_cache, UserPromocode, write)
	end,
    mnesia:activity(transaction, F).



