-module(user_service_db).

-include("../include/mysql.hrl").
-include("../include/user_service.hrl").
-include("../include/transaction.hrl").
-include("../include/partner.hrl").

%% Export functions for table `user_service`
-export([create/7, create/1, get/1, update/1]).
-export([set_status/2, get_first_purchases/3]).

%%%================= Функции работы с сервисами =================

create(UserId, ServiceId, NodeId, Params, CreateDate, EndDate, Status) ->
    emysql:prepare(create_user_service_stmt, <<"call `create_user_service`(?, ?, ?, ?, ?, ?, ?); ">>),  
    Result = emysql:execute(main_pool, create_user_service_stmt, [UserId, ServiceId, NodeId, Params, CreateDate, EndDate, Status]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[UserServiceId | _] | _] = ResultPacket#result_packet.rows,
		    US = user_service_db:get(#{user_service_id => UserServiceId}),
		    %%io:format("US : ~p~n", [US]),
		    US;
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.

create(#{user_id := UserId, service_id := ServiceId, node_id := NodeId, params := Params, create_date := CreateDate, end_date := EndDate, status := Status}) ->
    emysql:prepare(create_user_service_stmt, <<"call `create_user_service`(?, ?, ?, ?, ?, ?, ?); ">>),  
    Result = emysql:execute(main_pool, create_user_service_stmt, [UserId, ServiceId, NodeId, Params, CreateDate, EndDate, Status]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[UserServiceId | _] | _] = ResultPacket#result_packet.rows,
		    US = user_service_db:get(#{user_service_id => UserServiceId}),
		    %%io:format("US : ~p~n", [US]),
		    US;
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.


get(#{mode := count,
      filter_user_service_id := _FUserServiceId,
      filter_user_id := _FUserId,
      filter_user_service_status := _FUserServiceType,
      filter_service_type := _FServiceType,
      filter_create_date_from := _FCreateDateFrom,
      filter_create_date_to := _FCreateDateTo,
      filter_user_service_params_like := _FParamsLike,
      filter_order_by_dir := _FOrderByDir,
      filter_limit_offset := _FLimitOffset,
      filter_limit_rows := _FLimitRows
     } = Filters) ->
    
    io:format("DEBUG>>> user_service_db:get count Filters : ~p~n", [Filters]),
    Sql = build_count_query(Filters),

    io:format("DEBUG>>> user_service_db:get count_query sql : ~ts~n", [Sql]),
    emysql:prepare(get_count_user_services_by_filters_stmt, list_to_binary(Sql)),
    Result = emysql:execute(main_pool, get_count_user_services_by_filters_stmt, []),

    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
					  io:format("DEBUG>>> user_service_db:get count result row : ~p~n", [Row]),

					  [Count] = Row,

					  A = { [
						 {count, Count}
						] },
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    {ok, FinRows};
	_  -> {error, undefined}
    end;

get(#{filter_user_service_id := _FUserServiceId,
      filter_user_id := _FUserId,
      filter_user_service_status := _FUserServiceType,
      filter_service_type := _FServiceType,
      filter_create_date_from := _FCreateDateFrom,
      filter_create_date_to := _FCreateDateTo,
      filter_user_service_params_like := _FParamsLike,
      filter_order_by_dir := _FOrderByDir,
      filter_limit_offset := _FLimitOffset,
      filter_limit_rows := _FLimitRows
     } = Filters) ->

    io:format("DEBUG>>> user_service_db:get Filters : ~p~n", [Filters]),
    Sql = build_query(Filters),

    io:format("DEBUG>>> user_service_db:get query sql : ~ts~n", [Sql]),
    emysql:prepare(get_user_services_by_filters_stmt, list_to_binary(Sql)),
    Result = emysql:execute(main_pool, get_user_services_by_filters_stmt, []),

    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
					  [UserServiceId, CreateDate, Status, UserId, UserName, RemoteUserId, RemoteAuthType, ServiceType, ServiceName, ParamsBin] = Row,

					  Params = commons:decode_service_params(ParamsBin),

					  A = { [
						 {user_service_id, UserServiceId},
						 {create_date, CreateDate},
						 {status, Status},
						 {user_id, UserId},
						 {user_name, UserName},
						 {remote_user_id, RemoteUserId},
						 {remote_auth_type, RemoteAuthType},
						 {service_type, ServiceType},
						 {service_name, ServiceName},
						 {params, Params}
						] },
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    {ok, FinRows};
	_  -> {error, undefined}
    end;




get(#{limit_offset := LimitOffset, limit_rows := LimitRows}) ->
    emysql:prepare(get_user_service_limit,
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id ORDER BY us.`create_date` DESC
                      LIMIT ?,?">>),
    Result = emysql:execute(main_pool, get_user_service_limit, [LimitOffset, LimitRows]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)),
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{mode := full_count}) ->
    emysql:prepare(get_user_service_full_count, <<"SELECT COUNT(*) FROM `user_service`">>),
    Result = emysql:execute(main_pool, get_user_service_full_count, []),

    case Result of
	Result when is_record(Result, result_packet) ->
	    [CountRow] = Result#result_packet.rows,
	    [Count] = CountRow,
	    {ok, Count};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;




get(#{user_id := UserId, type := Type}) ->
    emysql:prepare(get_user_service_by_type_stmt,
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE `user_id` = ? AND s.`type` = ?">>),
    Result = emysql:execute(main_pool, get_user_service_by_type_stmt, [UserId, Type]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)), 
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};		
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{user_service_id := UserServiceId}) ->
    emysql:prepare(get_us_by_user_service_id_stmt, 
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE us.`user_service_id` = ?">>),
    Result = emysql:execute(main_pool, get_us_by_user_service_id_stmt, [UserServiceId]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)),
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(UserService)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{user_id := UserId}) ->
    emysql:prepare(get_user_service_by_user_id_stmt, 
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE us.`user_id` = ?">>),
    Result = emysql:execute(main_pool, get_user_service_by_user_id_stmt, [UserId]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)),
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{status := Status}) ->
    emysql:prepare(get_user_service_by_status_stmt,
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE us.`status` = ?">>),
    Result = emysql:execute(main_pool, get_user_service_by_status_stmt, [Status]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)), 

    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{lastwins := Length, item_type_filter := ItemTypeFilter}) ->
    WherePattern = case ItemTypeFilter of
		       knife ->
			   "(s.`type` != 91000 AND s.`type` != 57001) AND us.params LIKE '%knife%'";
		       covert ->
			   "(s.`type` != 91000 AND s.`type` != 57001) AND us.params LIKE '%covert%'";
		       {covert, knife} ->
			   "(s.`type` != 91000 AND s.`type` != 57001) AND (us.params LIKE '%covert%' OR us.params LIKE '%knife%')";
		       {classified, covert, knife} ->
			   "(s.`type` != 91000 AND s.`type` != 57001) AND (us.params LIKE '%classified%' OR us.params LIKE '%covert%' OR us.params LIKE '%knife%')";
		       {restricted, classified, covert, knife} ->
			   "(s.`type` != 91000 AND s.`type` != 57001) AND (us.params LIKE '%restricted%' OR us.params LIKE '%classified%' OR us.params LIKE '%covert%' OR us.params LIKE '%knife%')";
		       {milspec, restricted, classified, covert, knife} ->
			   "(s.`type` != 91000 AND s.`type` != 57001) AND (us.params LIKE '%milspec%' OR us.params LIKE '%restricted%' OR us.params LIKE '%classified%' OR us.params LIKE '%covert%' OR us.params LIKE '%knife%')"
		   end,
    SqlQuery = list_to_binary(io_lib:format("SELECT u.name, u.remote_user_id, u.remote_auth_type, us.params FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      LEFT JOIN `user` u ON us.user_id = u.user_id WHERE us.`create_date` > '2016-11-01 14:47:33' AND ~ts ORDER BY us.`create_date` DESC LIMIT 0,?", [WherePattern])),


    emysql:prepare(get_lastwins2_stmt, SqlQuery),

    Result = emysql:execute(main_pool, get_lastwins2_stmt, [Length]),

    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
					  [UserName, RemoteUserId, RemoteAuthType, ParamsBin] = Row,
					  Params = commons:decode_service_params(ParamsBin),

					  ItemName = proplists:get_value(item_name, Params),
					  ItemRarity = case proplists:get_value(rarity, Params) of
							   undefined ->
							       proplists:get_value(type, Params);
							   R -> R
						       end,
					  ItemImage = proplists:get_value(image, Params),

					  A = { [{user_name, UserName}, {remote_user_id, RemoteUserId}, {remote_auth_type, RemoteAuthType}, 
					       {item_name, ItemName}, {item_rarity, ItemRarity}, {item_image, ItemImage}] },
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    {ok, FinRows};
	_  -> {error, db_error}
    end;

get(#{lastwins := Length}) ->
    emysql:prepare(get_lastwins_stmt,
		   <<"SELECT u.name, u.remote_user_id, u.remote_auth_type, us.params FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      LEFT JOIN `user` u ON us.user_id = u.user_id WHERE s.`type` = 3001 OR s.`type` = 3002 ORDER BY us.`create_date` DESC LIMIT 0,?">>),

    Result = emysql:execute(main_pool, get_lastwins_stmt, [Length]),

    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
					  [UserName, RemoteUserId, RemoteAuthType, ParamsBin] = Row,
					  Params = commons:decode_service_params(ParamsBin),
					  ItemName = proplists:get_value(item_name, Params),
					  ItemRarity = case proplists:get_value(rarity, Params) of
							   undefined ->
							       proplists:get_value(type, Params);
							   R -> R
						       end,
					  ItemImage = proplists:get_value(image, Params),

					  A = { [{user_name, UserName}, {remote_user_id, RemoteUserId}, {remote_auth_type, RemoteAuthType}, 
					       {item_name, ItemName}, {item_rarity, ItemRarity}, {item_image, ItemImage}] },
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    {ok, FinRows};
	_  -> {error, db_error}
    end.


update(#{user_service_id := UserServiceId, user_id := UserId, status := Status}) ->
    emysql:prepare(update_user_service_3_stmt, <<"UPDATE `user_service` SET `user_id` = ?,`status` = ? WHERE `user_service_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_service_3_stmt, [UserId, Status, UserServiceId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end;

update(#{user_service_id := UserServiceId, params := Params}) ->
    emysql:prepare(update_user_service_1_stmt, <<"UPDATE `user_service` SET `params` = ? WHERE `user_service_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_service_1_stmt, [Params, UserServiceId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end;

update(#{user_service_id := UserServiceId, user_id := UserId}) ->
    emysql:prepare(update_user_service_2_stmt, <<"UPDATE `user_service` SET `user_id` = ? WHERE `user_service_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_service_2_stmt, [UserId, UserServiceId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end;

update(#{user_service_id := UserServiceId, status := Status}) ->
    emysql:prepare(update_user_service_3_stmt, <<"UPDATE `user_service` SET `status` = ? WHERE `user_service_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_service_3_stmt, [Status, UserServiceId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end.


%% Ф-я оставлена для совместимости
set_status(UserServiceId, Status) ->
    emysql:prepare(set_user_service_status_stmt, <<"UPDATE `user_service` SET `status` = ? WHERE `user_service_id` = ?">>),
    Result = emysql:execute(main_pool, set_user_service_status_stmt, [Status, UserServiceId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end.





get_first_purchases(Partner, FromDate, ToDate) ->
    %%io:format("get_first_purchases |  from=~p  to=~p~n", [FromDate, ToDate]),

    emysql:prepare(get_first_purchases_stmt, 
		   <<"SELECT `name`,`cost`,`date` 
FROM (SELECT u.name AS `name`, s.cost AS `cost`, us.create_date AS `date` FROM `user_service` us 
INNER JOIN `user` u ON u.user_id=us.user_id
INNER JOIN `service` s ON s.service_id=us.service_id 
WHERE u.`partner_id` = ?
ORDER BY `create_date` DESC) AS grp WHERE `date` BETWEEN ? AND ? ">>),

    Result = emysql:execute(main_pool, get_first_purchases_stmt, [Partner#partner.partner_id, FromDate, ToDate]),

    case Result of
	Result when is_record(Result, result_packet) -> 
	    FinRows = lists:foldl(fun(Row, Rows) -> 
					  [Name, Cost, Date] = Row,
					  A = [{name, Name}, {cost, Cost}, {date, Date}],
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    {ok, FinRows};
	_  -> {error, db_error}
    end.




%%%===================================================================
%%% Internal functions
%%%===================================================================


decode_params(Records) when is_list(Records) ->
    lists:foldl(
      fun(Record, R) ->
	      UpdatedRecord = decode_params(Record),
	      lists:append(R, [UpdatedRecord])
      end, [], Records);

decode_params(Record) when is_record(Record, user_service) ->
    TermParams = commons:decode_service_params(Record#user_service.params),
    %% модифицируем сруктуру сервиса
    Record#user_service{params=TermParams}.







get_filter_sql_expr(#{filter_user_service_id := undefined}) -> undefined;
get_filter_sql_expr(#{filter_user_service_id := FUserServiceId}) -> 
    lists:flatten(io_lib:format("us.`user_service_id` = ~p", [FUserServiceId]));



get_filter_sql_expr(#{filter_user_id := undefined}) -> undefined;
get_filter_sql_expr(#{filter_user_id := FUserId}) -> 
    lists:flatten(io_lib:format("us.`user_id` = ~p", [FUserId]));



%% Функция строит SQL условие для выборки по статусу сервиса
get_filter_sql_expr(#{filter_user_service_status := undefined}) -> undefined;
get_filter_sql_expr(#{filter_user_service_status := FUserServiceStatuses}) when is_list(FUserServiceStatuses) -> 
    %% Сформировать список OR токенов
    Tokens = lists:foldr(fun(FUserServiceStatus, Res) ->
				 T = lists:flatten(io_lib:format("us.`status` = ~p", [FUserServiceStatus])),
				 lists:append(Res, [{" OR ", T}])
			 end, [], FUserServiceStatuses),
    Expr = sql_builder:build_expr(Tokens),
    %% OR конструкции заключаем в скобки
    lists:flatten(io_lib:format("(~ts)", [Expr]));

get_filter_sql_expr(#{filter_user_service_status := FUserServiceStatus}) -> 
    lists:flatten(io_lib:format("us.`status` = ~p", [FUserServiceStatus]));



%% Функция строит SQL условие для выборки по типу сервиса
get_filter_sql_expr(#{filter_service_type := undefined}) -> undefined;
get_filter_sql_expr(#{filter_service_type := FServiceTypes}) when is_list(FServiceTypes) -> 
    %% Сформировать список OR токенов
    Tokens = lists:foldr(fun(FServiceType, Res) ->
				 T = lists:flatten(io_lib:format("s.`type` = ~p", [FServiceType])),
				 lists:append(Res, [{" OR ", T}])
			 end, [], FServiceTypes),
    Expr = sql_builder:build_expr(Tokens),
    %% OR конструкции заключаем в скобки
    lists:flatten(io_lib:format("(~ts)", [Expr]));

get_filter_sql_expr(#{filter_service_type := FServiceType}) -> 
    lists:flatten(io_lib:format("s.`type` = ~p", [FServiceType]));



%% Функция строит SQL условие для выборки от даты создания
get_filter_sql_expr(#{filter_create_date_from := undefined}) -> undefined;
get_filter_sql_expr(#{filter_create_date_from := FCreateDateFrom}) -> 
    {{Year,Month,Day},{Hour,Min,Sec}} = FCreateDateFrom, %% Datetime
    Date = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
			 [Year, Month, Day, Hour, Min, Sec])),
    lists:flatten(io_lib:format("us.`create_date` > '~ts'", [Date]));

%% Функция строит SQL условие для выборки до даты создания
get_filter_sql_expr(#{filter_create_date_to := undefined}) -> undefined;
get_filter_sql_expr(#{filter_create_date_to := FCreateDateTo}) -> 
    {{Year,Month,Day},{Hour,Min,Sec}} = FCreateDateTo, %% Datetime
    Date = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
			 [Year, Month, Day, Hour, Min, Sec])),
    lists:flatten(io_lib:format("us.`create_date` < '~ts'", [Date]));



%get_filter_sql_expr(#{filter_user_service_params_like := FUserServiceParamsLike}) -> 
%    lists:flatten(io_lib:format("us.`params` LIKE '%~ts%'", [FUserServiceParamsLike]));

%% Функция строит SQL условие для LIKE выборки по параметрам сервиса
get_filter_sql_expr(#{filter_user_service_params_like := undefined}) -> undefined;
get_filter_sql_expr(#{filter_user_service_params_like := FUserServiceParamsLike}) when is_list(FUserServiceParamsLike) -> 
    %% Сформировать список OR токенов
    Tokens = lists:foldr(fun({FUserServiceParamLike}, Res) ->
				 T = lists:flatten(io_lib:format("us.`params` LIKE '%~ts%'", [FUserServiceParamLike])),
				 lists:append(Res, [{" OR ", T}])
			 end, [], FUserServiceParamsLike),
    Expr = sql_builder:build_expr(Tokens),
    %% OR конструкции заключаем в скобки
    lists:flatten(io_lib:format("(~ts)", [Expr]));


%% Функция строит SQL задающий направление сортиировки
get_filter_sql_expr(#{filter_order_by_dir := undefined}) -> "";
get_filter_sql_expr(#{filter_order_by_dir := desc}) -> 
    lists:flatten(io_lib:format("DESC", []));
get_filter_sql_expr(#{filter_order_by_dir := asc}) -> 
    lists:flatten(io_lib:format("ASC", []));

%% Функция строит SQL конструкции LIMIT
get_filter_sql_expr(#{filter_limit_offset := undefined, filter_limit_rows := _}) -> "";
get_filter_sql_expr(#{filter_limit_offset := _, filter_limit_rows := undefined}) -> "";
get_filter_sql_expr(#{filter_limit_offset := FLimitOffset, filter_limit_rows := FLimitRows}) -> 
    lists:flatten(io_lib:format("LIMIT ~p,~p", [FLimitOffset, FLimitRows])).



build_count_query(Filters) ->
    #{
       filter_user_service_id := FUserServiceId,
       filter_user_id := FUserId,
       filter_user_service_status := FUserServiceStatus,
       filter_service_type := FServiceType,
       filter_create_date_from := FCreateDateFrom,
       filter_create_date_to := FCreateDateTo,
       filter_user_service_params_like := FUserServiceParamsLike,
       filter_order_by_dir := FOrderByDir,
       filter_limit_offset := FLimitOffset,
       filter_limit_rows := FLimitRows
     } = Filters,

    WhereF1 = get_filter_sql_expr(#{filter_user_service_id => FUserServiceId}),
    WhereF2 = get_filter_sql_expr(#{filter_user_id => FUserId}),
    WhereF3 = get_filter_sql_expr(#{filter_user_service_status => FUserServiceStatus}),
    WhereF4 = get_filter_sql_expr(#{filter_service_type => FServiceType}),
    WhereF5 = get_filter_sql_expr(#{filter_create_date_from => FCreateDateFrom}),
    WhereF6 = get_filter_sql_expr(#{filter_create_date_to => FCreateDateTo}),
    WhereF7 = get_filter_sql_expr(#{filter_user_service_params_like => FUserServiceParamsLike}),

    OrderDirSqlExpr = get_filter_sql_expr(#{filter_order_by_dir => FOrderByDir}),
    LimitSqlExpr = get_filter_sql_expr(#{filter_limit_offset => FLimitOffset, filter_limit_rows => FLimitRows}),

    %%io:format("DEBUG >>> build_query LimitSqlExpr = ~p~n", [LimitSqlExpr]),

    WhereTokens = [
		    {" AND ", WhereF1},
		    {" AND ", WhereF2},
		    {" AND ", WhereF3},
		    {" AND ", WhereF4},
		    {" AND ", WhereF5},
		    {" AND ", WhereF6},
		    {" AND ", WhereF7}
		  ],
    
    Where = sql_builder:build_expr(WhereTokens),
    
    io:format("DEBUG >>> build_count_query Where = ~p~n", [Where]),
    
    Sql = lists:flatten(io_lib:format(
			  " SELECT "
			  " COUNT(*) as `count` "
			  " FROM `user_service` us "
			  " INNER JOIN `service` s ON us.service_id=s.service_id "
			  " LEFT JOIN `user` u ON us.user_id=u.user_id "
			  " WHERE ~ts ORDER BY us.create_date ~ts ~ts", [Where, OrderDirSqlExpr, LimitSqlExpr])),
    Sql.

	
build_query(Filters) ->
    #{
       filter_user_service_id := FUserServiceId,
       filter_user_id := FUserId,
       filter_user_service_status := FUserServiceStatus,
       filter_service_type := FServiceType,
       filter_create_date_from := FCreateDateFrom,
       filter_create_date_to := FCreateDateTo,
       filter_user_service_params_like := FUserServiceParamsLike,
       filter_order_by_dir := FOrderByDir,
       filter_limit_offset := FLimitOffset,
       filter_limit_rows := FLimitRows
     } = Filters,

    WhereF1 = get_filter_sql_expr(#{filter_user_service_id => FUserServiceId}),
    WhereF2 = get_filter_sql_expr(#{filter_user_id => FUserId}),
    WhereF3 = get_filter_sql_expr(#{filter_user_service_status => FUserServiceStatus}),
    WhereF4 = get_filter_sql_expr(#{filter_service_type => FServiceType}),
    WhereF5 = get_filter_sql_expr(#{filter_create_date_from => FCreateDateFrom}),
    WhereF6 = get_filter_sql_expr(#{filter_create_date_to => FCreateDateTo}),
    WhereF7 = get_filter_sql_expr(#{filter_user_service_params_like => FUserServiceParamsLike}),

    OrderDirSqlExpr = get_filter_sql_expr(#{filter_order_by_dir => FOrderByDir}),
    LimitSqlExpr = get_filter_sql_expr(#{filter_limit_offset => FLimitOffset, filter_limit_rows => FLimitRows}),

    %%io:format("DEBUG >>> build_query LimitSqlExpr = ~p~n", [LimitSqlExpr]),

    WhereTokens = [
		    {" AND ", WhereF1},
		    {" AND ", WhereF2},
		    {" AND ", WhereF3},
		    {" AND ", WhereF4},
		    {" AND ", WhereF5},
		    {" AND ", WhereF6},
		    {" AND ", WhereF7}
		  ],
    
    Where = sql_builder:build_expr(WhereTokens),
    
    %%io:format("DEBUG >>> build_query Where = ~p~n", [Where]),
    
    Sql = lists:flatten(io_lib:format(
			  " SELECT "
			  " us.user_service_id as 'user_service_id', "
			  " us.create_date as 'create_date', "
			  " us.status   as 'status', "
			  " us.user_id  as 'user_id', "
			  " u.name      as 'user_name', "
			  " u.remote_user_id  as 'remote_user_id', "
			  " u.remote_auth_type  as 'remote_auth_type', "
			  " s.type      as 'service_type', "
			  " s.name      as 'service_name', "
			  " us.params   as 'params' "
			  " FROM `user_service` us "
			  " INNER JOIN `service` s ON us.service_id=s.service_id "
			  " LEFT JOIN `user` u ON us.user_id=u.user_id "
			  " WHERE ~ts ORDER BY us.create_date ~ts ~ts", [Where, OrderDirSqlExpr, LimitSqlExpr])),
    Sql.



%% fix_user_service() ->
%%     %% Получить список всех пользовательских сервисов с user_id = 0
%%     {ok, UserServices} = user_service_db:get(#{user_id => 0}),
%%     io:format("Corrupt user services array lenght ~p~n", [length(UserServices)]),
%%     lists:foreach(fun(UserService) ->
%% 			  CloseDate1 = UserService#user_service.create_date,
%% 			  {datetime, {{Y,MON,DATE},{H,M,S}}} = CloseDate1,
%% 			  CloseDate2 = {datetime, {{Y,MON,DATE},{H,M,S-1}}},
%% 			  %% Найти транзакцию оплаты этого заказа
%% 			  Res = transaction_db:get(#{type => 1, close_date => CloseDate1}),
%% 			  case Res of
%% 			      {ok, [Tr]} ->
%% 				  io:format("Transaction^ finded! user_service_id=~p  user_id=~p~n", [UserService#user_service.user_service_id, Tr#transaction.user_id]),
%% 				  user_service_db:update(#{user_service_id => UserService#user_service.user_service_id, user_id => Tr#transaction.user_id}),
%% 				  ok;
%% 			      {ok, [Tr1, Tr2]} ->
%% 				  io:format("Transaction^ double finded! user_service_id=~p  user_id 1=~p user_id 2~p~n", [UserService#user_service.user_service_id, Tr1#transaction.user_id, Tr2#transaction.user_id]),
%% 				  ok;
%% 			      Res ->
%% 				  %% Найти транзакцию оплаты этого заказа за секунду `до`
%% 				  Res2 = transaction_db:get(#{type => 1, close_date => CloseDate2}),
%% 				  case Res2 of 
%% 				      {ok, [Tr]} ->
%% 					  io:format("Transaction^^ finded! user_service_id=~p  user_id=~p~n", [UserService#user_service.user_service_id, Tr#transaction.user_id]),
%% 					  user_service_db:update(#{user_service_id => UserService#user_service.user_service_id, user_id => Tr#transaction.user_id}),
%% 					  ok;
%% 				      {ok, [Tr1, Tr2]} ->
%% 					  io:format("Transaction^^ double finded! user_service_id=~p  user_id 1=~p user_id 2~p~n", [UserService#user_service.user_service_id, Tr1#transaction.user_id, Tr2#transaction.user_id]),
%% 					  ok;
%% 				      Res2 ->
%% 					  io:format("Transaction^^ not finded :( Res = ~p~n", [Res])
%% 				  end
				  
%% 			  end
%% 	    end, 
%% 	    UserServices).
