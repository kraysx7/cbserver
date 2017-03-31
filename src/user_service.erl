-module(user_service).

-include("../include/user_service.hrl").
-include("../include/user_service_param.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export([create/6, create/1, get/1, update/1, set_status/2]).

-export([insert_cache/1, insert_param_cache/1]).

create(UserId, ServiceId, NodeId, Params, EndDate, Status) ->
    CreateDate = calendar:local_time(),
    ParamsStr = lists:flatten(io_lib:format("~p.", [Params])),
    user_service_db:create(UserId, ServiceId, NodeId, ParamsStr, CreateDate, EndDate, Status).


create(#{user_id := UserId, service_id := ServiceId, node_id := NodeId, params := Params, end_date := EndDate, status := Status}) ->
    CreateDate = calendar:local_time(),
    ParamsStr = lists:flatten(io_lib:format("~p.", [Params])),
    user_service_db:create(UserId, ServiceId, NodeId, ParamsStr, CreateDate, EndDate, Status).


%% get(#{
%%       filter_user_service_id := _FUserServiceId,
%%       filter_user_id := _FUserId,
%%       filter_user_service_status := _FUserServiceType,
%%       filter_service_type := _FServiceType,
%%       filter_create_date_from := FCreateDateFrom,
%%       filter_create_date_to := FCreateDateTo,
%%       filter_user_service_params := _FParams,
%%       filter_order_by_dir := _FOrderByDir,
%%       filter_limit_offset := _FLimitOffset,
%%       filter_limit_rows := _FLimitRows
%%      } = Filters) ->
%%     %% Получить из кэша данные в заданом диапазоне дат
%%     %% Сначала выбираем из кэша данные по таблице user_service c заданными фильтрами
%%     io:format("DEBUG >>> user_service:get filters ~p~n", [Filters]),

%%     %% Преобразовать datetime в timestamp
%%     FCreateTimestampFrom = case FCreateDateFrom of
%% 			       undefined -> undefined;
%% 			       {{_,_,_},{_,_,_}} = DT1 ->
%% 				   calendar:datetime_to_gregorian_seconds(DT1)
%% 			   end,
%%     FCreateTimestampTo = case FCreateDateTo of
%% 			     undefined -> undefined;
%% 			     {{_,_,_},{_,_,_}} = DT2 ->
%% 				 calendar:datetime_to_gregorian_seconds(DT2)
%% 			 end,

%%     CacheFilters = maps:merge(Filters, #{filter_create_timestamp_from => FCreateTimestampFrom, filter_create_timestamp_to => FCreateTimestampTo}),

%%     io:format("DEBUG >>> user_service:get CacheFilters ~p~n", [CacheFilters]),

%%     {ok, get_cache(CacheFilters)};

get(#{user_service_id := UserServiceId, mode := cache}) ->
    %% Получить user_service из кэша mnesia
    CacheUserServiceRes =get_cache(#{user_service_id => UserServiceId}),
    %% io:format("DEBUG>>> user_service:get() get_cache res=~p~n", [CacheUserServiceRes]),
    case CacheUserServiceRes of
	%% Если записи нет, получаем объект из базы и помещаем в кэш
	[] ->
	    case user_service_db:get(#{user_service_id => UserServiceId}) of
		{ok, UserService} ->
		    EndDateTimestamp0 = datetime_to_timestamp(UserService#user_service.end_date),
		    FinUserService = UserService#user_service{end_date=EndDateTimestamp0},
		    insert_cache(FinUserService),
		    {ok, FinUserService};
		{error, not_found} ->
		    {error, not_found}
	    end;
	[CacheUserService] when is_record(CacheUserService, user_service) ->
	    {ok, CacheUserService}
    end;

get(ParamsMap) ->
    user_service_db:get(ParamsMap).


update(#{user_service_id := UserServiceId, old_status := OldStatus, new_status := NewStatus, end_date := EndDateTimestamp, mode := cache}) ->
    %% Получить user_service из кэша mnesia
    CacheUserServiceRes =get_cache(#{user_service_id => UserServiceId}),
    %% io:format("DEBUG>>> user_service:update() get_cache res: ~p~n", [CacheUserServiceRes]),
    
    case CacheUserServiceRes of
	%% Если записи нет, получаем объект из базы и помещаем в кэш
	[] ->
	    case user_service_db:get(#{user_service_id => UserServiceId}) of
		{ok, UserService} ->
		    EndDateTimestamp0 = datetime_to_timestamp(UserService#user_service.end_date),
		    FinUserService = UserService#user_service{end_date=EndDateTimestamp0},
		    insert_cache(FinUserService),
		    R = update_cache(#{user_service_id => UserServiceId, old_status => OldStatus, new_status => NewStatus, end_date => EndDateTimestamp0}),
		    %% io:format("DEBUG>>> user_service:update() update_cache1 res: ~p~n", [R]),
		    R;
		{error, not_found} ->
		    {error, not_found}
	    end;
	[CacheUserService] when is_record(CacheUserService, user_service),CacheUserService#user_service.status==OldStatus ->
	    R = update_cache(#{user_service_id => UserServiceId, old_status => OldStatus, new_status => NewStatus, end_date => EndDateTimestamp}),
	    %% io:format("DEBUG>>> user_service:update() update_cache2 res: ~p~n", [R]),
	    R;
	[CacheUserService] when is_record(CacheUserService, user_service) ->
	    {error, {bad_old_status, CacheUserService#user_service.status}}
    end;

	   
update(#{user_service_id := _UserServiceId, params := UserServiceParams} = ParamsMap) ->
    UserServiceParamsStr = lists:flatten(io_lib:format("~p.", [UserServiceParams])),
    UpdatedParamsMap = maps:update(params, UserServiceParamsStr, ParamsMap),
    user_service_db:update(UpdatedParamsMap);

update(ParamsMap) ->
    user_service_db:update(ParamsMap).

set_status(UserServiceId, Status) ->
    user_service_db:set_status(UserServiceId, Status).







%% MNESIA API %%

insert_cache(UserService) ->
    %%io:format("DEBUG>>> user_service:insert_cache pid: ~p  us: ~p~n", [self(), UserService#user_service.user_service_id]),
    F = fun() -> mnesia:write(user_service_cache, UserService, write) end,
    mnesia:activity(transaction, F).

insert_param_cache(#{mode := normal, param := UserServiceParam}) ->
    F = fun() -> mnesia:write(user_service_params_cache, UserServiceParam, write) end,
    mnesia:activity(transaction, F);

insert_param_cache(#{mode := durty, param := UserServiceParam}) ->
    F = fun() -> mnesia:write(user_service_params_cache, UserServiceParam, write) end,
    mnesia:activity(async_dirty, F).




update_cache(#{user_service_id := UserServiceId, old_status := OldStatus, new_status := NewStatus, end_date := EndDateTimestamp}) ->
    Pattern = #user_service{_ = '_', user_service_id = UserServiceId},
    F = fun() -> 
 		case mnesia:match_object(user_service_cache, Pattern, write) of
		    [] -> {error, not_found};
 		    [UserService] when is_record(UserService, user_service),UserService#user_service.status==OldStatus->
			NewUserService = UserService#user_service{status=NewStatus, end_date=EndDateTimestamp},
			mnesia:write(user_service_cache, NewUserService, write),
			{ok, NewUserService};
		    [UserService] when is_record(UserService, user_service) ->
			{error, {bad_old_status, UserService#user_service.status}}
 		end
	end,
    mnesia:activity(transaction, F).


get_cache(#{user_service_id := UserServiceId}) ->
    F = fun() -> mnesia:read({user_service_cache, UserServiceId})  end,
    mnesia:activity(transaction, F);

%% get_cache(ParamsMap) -> list()
%% Функция получает из кэша срез данных на основе заданных фильтров
%%
%% Основные параметры :
%% filter_user_service_id     -> integer()
%% filter_user_id             -> integer()
%% filter_user_service_status -> integer()
%% filter_service_type        -> integer()
%% filter_create_date_from    -> integer() 
%% filter_create_date_to      -> integer()
%% filter_user_service_params -> [{key, val}, {key, val}]

%% Логические страничные фильтры :
%% filter_order_by_dir        -> datetime()
%% filter_limit_offset        -> integer()
%% filter_limit_rows          -> integer()
get_cache(#{
	     filter_user_service_id := _FUserServiceId,
	     filter_user_id := _FUserId,
	     filter_user_service_status := _FUserServiceStatus,
	     filter_service_type := _FServiceType,
	     filter_create_timestamp_from := _FCreateTimestampFrom,
	     filter_create_timestamp_to := _FCreateTimestampTo,
	     filter_user_service_params := FParams,

	     filter_order_by := FOrderBy,
	     filter_order_by_dir := _FOrderByDir,
	     filter_limit_offset := FLimitOffset,
	     filter_limit_rows := FLimitRows
	   } = Filters) ->
    io:format("DEBUG >>> user_service:get_cache filters ~p~n", [Filters]),
    F = fun() ->
		QH_US1 = build_qlc_query(Filters),
		US1 = qlc:e(QH_US1),
		
		%% Формируем функцию сортировки
		F = case FOrderBy of
			price -> 
			    fun(X, Y) -> 
				    P1 = proplists:get_value(price, X#user_service.params),
				    P2 = proplists:get_value(price, Y#user_service.params),
				    case {P1, P2} of
					{undefined, _} -> true;
					{_, undefined} -> false;
					{P1, P2} -> P1 > P2
				    end
			    end;
			create_date ->
			    fun(X, Y) -> 
				    P1 = X#user_service.create_date,
				    P2 = Y#user_service.create_date,
				    case {P1, P2} of
					{undefined, _} -> true;
					{_, undefined} -> false;
					{P1, P2} -> P1 > P2
				    end
			    end;
			_ -> undefined
		    end,
		L = case F of
			undefined -> US1;
			F ->
			    lists:sort(F, US1)
		    end,

		%% Берём нужный кусочек
		UserServices = lists:sublist(L, FLimitOffset, FLimitRows),

		%%io:format("UserServices lenght: ~p~n", [length(UserServices)]),
		%%io:format("US1 lenght: ~p~n", [length(US1)]),

		%% Возвращаем страницу и общее количество записей
		{UserServices, length(US1)}

	end,
    mnesia:activity(transaction, F).

%% QLC build functions

build_qlc_query(#{filter_user_service_id := undefined,
		  filter_user_id := undefined,
		  filter_user_service_status := undefined,
		  filter_service_type := undefined,
		  filter_create_timestamp_from := undefined,
		  filter_create_timestamp_to := undefined}) ->
    qlc:q([US || US <- mnesia:table(user_service_cache)]);


build_qlc_query(#{
		   filter_user_service_id := FUserServiceId,
		   filter_user_id := FUserId,
		   filter_user_service_status:= FUserServiceStatus,
		   filter_service_type := FServiceType,
		   filter_create_timestamp_from := FCreateTimestampFrom,
		   filter_create_timestamp_to := FCreateTimestampTo,
		   filter_user_service_params := FParams
		 } = Params) ->
    io:format("DEBUG >>> user_service:build_qlc_query ~p~n", [Params]),
    qlc:q([US || US <- mnesia:table(user_service_cache),
		 build_user_service_id_expr(US, FUserServiceId),
		 build_user_id_expr(US, FUserId),
		 build_user_service_status_expr(US, FUserServiceStatus),
		 build_service_type_expr(US, FServiceType),
		 build_create_timestamp_from(US, FCreateTimestampFrom),
		 build_create_timestamp_to(US, FCreateTimestampTo),
		 build_params_filter_expr(US, FParams)
	  ]).


build_user_service_id_expr(UserService, FUserServiceId) ->
    case FUserServiceId of
	undefined -> true;
	_ -> UserService#user_service.user_service_id == FUserServiceId
    end.

build_user_id_expr(UserService, FUserId) ->
    case FUserId of
	undefined -> true;
	_ -> UserService#user_service.user_id == FUserId
    end.


build_user_service_status_expr(UserService, FUserServiceStatus) when is_list(FUserServiceStatus)->
    case FUserServiceStatus of
	[] -> true;
	_ -> lists:member(UserService#user_service.status, FUserServiceStatus)
    end;
build_user_service_status_expr(UserService, FUserServiceStatus)  when is_integer(FUserServiceStatus) ->
    case FUserServiceStatus of
	undefined -> true;
	_ -> UserService#user_service.status == FUserServiceStatus
    end.



build_service_type_expr(UserService, FServiceType) when is_list(FServiceType)->
    case FServiceType of
	[] -> true;
	_ -> lists:member(UserService#user_service.type, FServiceType)
    end;
build_service_type_expr(UserService, FServiceType) when is_integer(FServiceType) ->
    case FServiceType of
	undefined -> true;
	_ -> UserService#user_service.type == FServiceType
    end.



build_create_timestamp_from(UserService, FCreateTimestampFrom) ->
    case FCreateTimestampFrom of
	undefined -> true;
	_ -> UserService#user_service.create_date >= FCreateTimestampFrom
    end.

build_create_timestamp_to(UserService, FCreateTimestampTo) ->
    case FCreateTimestampTo of
	undefined -> true;
	_ -> UserService#user_service.create_date < FCreateTimestampTo
    end.



build_params_filter_expr(UserService, FParams) when is_list(FParams)->
    case FParams of
 	[] -> true;
	_ ->
 	    lists:foldl(fun(FParam, Res) ->
 				case Res of
 				    true -> true;
 				    false ->
					case FParam of
					    {FParamKey, gt, FParamValue} ->
						case proplists:get_value(FParamKey, UserService#user_service.params) of
						    undefined -> false;
						    ParamValue when ParamValue > FParamValue -> true;
						    _ -> false
						end;
					    {FParamKey, FParamValue} ->
						case proplists:get_value(FParamKey, UserService#user_service.params) of
						    undefined -> false;
						    FParamValue -> true;
						    _ -> false
						end
					end
				end
 			end, false, FParams)
    end;
build_params_filter_expr(UserService, FParam) ->
    case FParam of
	undefined -> true;
	{FParamKey, FParamValue} ->
	    UsParamValue = proplists:get_value(FParamKey, UserService#user_service.params),

	    case UsParamValue of
		undefined -> false;
		FParamValue -> true;
		_ -> false
	    end;
	_ -> false
    end.



datetime_to_timestamp(Datetime) ->
    FEndTimestamp = case Datetime of
			undefined -> undefined;
			{datetime, {{_,_,_},{_,_,_}} = DT2} ->
			    calendar:datetime_to_gregorian_seconds(DT2)
		    end,
    FEndTimestamp.








		%% %% Получаем все заказы, у которых есть хотя-бы 1 параметр из списка
		%% QH_US2 = qlc:q([US || US <- QH_US1,
		%% 		      US_Param <- mnesia:table(user_service_params_cache),
		%% 		      US#user_service.user_service_id == US_Param#user_service_param.user_service_id,
		%% 		      build_params_filter_expr(US_Param, FParams)
		%% 	       ]),

		%% %% Фильтруем по параметрам
		%% lists:foldl(fun(UserService, Res) -> 

		%% 		    %% Получить из кэша все параметры подкл. услуги
		%% 		    UserServiceParams = get_user_service_params(#{user_service_id => UserService#user_service.user_service_id}),
				    
		%% 		    %% Сравнить параметры и фильтр
				    
		%%  		    %% Если хоть одного фильтр-параметра нет в параметрах предмета - это не наш предмет
		%%  		    %% По `умолчанию` считаем что предмет нам подходит
		%%  		    IsTagExists = lists:foldl(fun(F_Param, R) -> 
		%%  		     				      case R of
		%%  		     					  false -> false;
		%%  		     					  true -> check_user_service_param(#{us_params => UserServiceParams, filter_param => F_Param})
		%%  		     				      end
		%%  		     			      end,
		%%  		     			      true, FParams),
		%%  		    case IsTagExists of
		%%  		     	true -> lists:flatten(lists:append(FinItems, [Item]));
		%%  		     	false -> FinItems
		%% 		    end
		%%  	    end, [], UserServices)
