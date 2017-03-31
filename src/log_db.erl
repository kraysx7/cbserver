-module(log_db).

-include("../include/mysql.hrl").
-include("../include/user.hrl").
-include("../include/log.hrl").

-export([create/1, get/1, debug/0, get_templates/0]).

%%%================= Функции работы с журналом =================

create(#{log_desc_id := LogDescId,
         log_template_params := LogTemplateParams,
         user_id := UserId,
	 subject_ip := Ip
	} = Map) ->
    io:format("create log entry : ~p~n", [Map]),
    emysql:prepare(create_log_entry_stmt, <<"call `create_log_entry`(?, ?, ?, ?);">>),

    Result = emysql:execute(main_pool, create_log_entry_stmt, [LogDescId, LogTemplateParams, UserId, Ip]),

    io:format("~p~n", [Result]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[LogId | _] | _] = ResultPacket#result_packet.rows,
		    log_db:get(#{log_id => LogId});
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.

debug() ->
    Sql = build_query(#{
       filter_access_level => undefined,
       filter_log_id => undefined,
       filter_log_desc_id => 1,
       filter_user_id => undefined,
       filter_subject_ip => undefined,
       filter_create_date_from => undefined,
       filter_create_date_to => undefined,
       filter_log_template_params_like => undefined,
       filter_order_by_dir => undefined,
       filter_limit_offset => undefined,
       filter_limit_rows => undefined
    }),
    
    io:format("DEBUG>>> log_db query sql : ~ts~n", [Sql]),
    emysql:prepare(get_logs_by_filters_stmt, list_to_binary(Sql)),
    Result = emysql:execute(main_pool, get_logs_by_filters_stmt, []),

    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
					  [LogId, Template, ParamsBin, CreateDate, SubjectIp, UserName, UserId] = Row,
					  
					  Params = ParamsBin,%%commons:decode_service_params(ParamsBin),
					  
					  A = #{
					    log_id => LogId,
					    template => Template,
					    params => Params,
					    create_date => CreateDate,
					    subject_ip => SubjectIp,
					    username => UserName,
					    user_id => UserId
					   },
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    io:format("debug FinRows: ~p~n", [FinRows]),
	    {ok, FinRows};
	_  -> {error, undefined}
    end.


get(#{
       filter_access_level := _FAccessLevel,
       filter_log_id := _FLogId,
       filter_log_desc_id := _FLogDescId,
       filter_user_id := _FUserId,
       filter_subject_ip := _FSubjectIP,
       filter_create_date_from := _FCreateDateFrom,
       filter_create_date_to := _FCreateDateTo,
       filter_log_template_params_like := _FLogTemplateParamsLike,
       filter_order_by_dir := _FOrderByDir,
       filter_limit_offset := _FLimitOffset,
       filter_limit_rows := _FLimitRows
     } = Filters) ->
    io:format("DEBUG>>>> log_db Filters : ~p~n", [Filters]),
    Sql = build_query(Filters),
    
    io:format("DEBUG>>> log_db query sql : ~ts~n", [Sql]),
    emysql:prepare(get_logs_by_filters_stmt, list_to_binary(Sql)),
    Result = emysql:execute(main_pool, get_logs_by_filters_stmt, []),

    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
					  [LogId, LogDescId, ParamsBin, CreateDate, SubjectIp, UserName, UserId] = Row,
					  
					  Params = ParamsBin,%%commons:decode_service_params(ParamsBin),
					  
					  A = #{
					    log_id => LogId,
					    log_desc_id => LogDescId,
					    params => Params,
					    create_date => CreateDate,
					    subject_ip => SubjectIp,
					    username => UserName,
					    user_id => UserId
					   },
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    io:format("debug FinRows: ~p~n", [FinRows]),
	    {ok, FinRows};
	_  -> {error, undefined}
    end;

get(#{log_id := LogId}) ->
    emysql:prepare(get_log_entry_by_id_stmt, <<"SELECT * FROM `log` WHERE `log_id` = ?">>),
    Result = emysql:execute(main_pool, get_log_entry_by_id_stmt, [LogId]),
    RecResult = emysql:as_record(Result, log, record_info(fields, log)), 
    case RecResult of
	[LogEntry | _] when is_record(LogEntry, log) -> {ok, decode_params(LogEntry)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


get_templates() ->
    emysql:prepare(get_log_templates_stmt, <<"SELECT log_desc_id, template_ru FROM `log_desc`">>),
    Result = emysql:execute(main_pool, get_log_templates_stmt, []),
    case Result of
	Result when is_record(Result, result_packet) ->
	    FinRows = lists:foldl(fun(Row, Rows) ->
						[LogDescId, Template] = Row,
						lists:append([{LogDescId, Template}], Rows)
						end, [], Result#result_packet.rows),
					{ok, FinRows};
				_ ->
					{error, undefined}
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

decode_params(Record) when is_record(Record, log) ->
    TermParams = commons:decode_service_params(Record#log.log_template_params),
    %% модифицируем сруктуру сервиса
    Record#log{log_template_params=TermParams}.


get_filter_sql_expr(#{filter_access_level := undefined}) -> undefined;
get_filter_sql_expr(#{filter_access_level := FAccessLevel}) -> 
    lists:flatten(io_lib:format("ld.`access_level` <= ~p", [FAccessLevel]));

get_filter_sql_expr(#{filter_log_id := undefined}) -> undefined;
get_filter_sql_expr(#{filter_log_id := FLogId}) -> 
    lists:flatten(io_lib:format("l.`log_id` = ~p", [FLogId]));

get_filter_sql_expr(#{filter_log_desc_id := undefined}) -> undefined;
get_filter_sql_expr(#{filter_log_desc_id := FLogDescId}) -> 
    lists:flatten(io_lib:format("l.`log_desc_id` = ~p", [FLogDescId]));

get_filter_sql_expr(#{filter_user_id := undefined}) -> undefined;
get_filter_sql_expr(#{filter_user_id := FUserId}) -> 
    lists:flatten(io_lib:format("l.`user_id` = ~p", [FUserId]));

get_filter_sql_expr(#{filter_subject_ip := undefined}) -> undefined;
get_filter_sql_expr(#{filter_subject_ip := FSubjectIP}) -> 
    lists:flatten(io_lib:format("l.`subject_ip` = INET6_ATON('~ts')", [FSubjectIP]));

get_filter_sql_expr(#{filter_user_id := undefined}) -> undefined;
get_filter_sql_expr(#{filter_user_id := FUserId}) -> 
    lists:flatten(io_lib:format("l.`user_id` = ~p", [FUserId]));


%%    Закоментирован вариант с использованием erlang-формата даты. В данном случае дата сразу задается в mysql-формате.
%%get_filter_sql_expr(#{filter_create_date_from := undefined, filter_create_date_to := undefined}) -> undefined;
%%get_filter_sql_expr(#{filter_create_date_from := FCreateDateFrom, filter_create_date_to := undefined}) -> 
%%    {{Year,Month,Day},{Hour,Min,Sec}} = FCreateDateFrom, %% Datetime
%%    Date = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%%			 [Year, Month, Day, Hour, Min, Sec])),
%%    lists:flatten(io_lib:format("`create_date` BETWEEN STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s') AND STR_TO_DATE(`2999-12-31 15:59:59`, '%Y-%m-%d %H:%i:%s')", [Date]));
%%    
%%get_filter_sql_expr(#{filter_create_date_from := undefined, filter_create_date_to := FCreateDateTo}) -> 
%%    {{Year,Month,Day},{Hour,Min,Sec}} = FCreateDateTo, %% Datetime
%%    Date = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%%			 [Year, Month, Day, Hour, Min, Sec])),
%%    lists:flatten(io_lib:format("`create_date` BETWEEN STR_TO_DATE('1999-12-31 15:59:59', '%Y-%m-%d %H:%i:%s') AND STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s')", [Date]));
%%
%%get_filter_sql_expr(#{filter_create_date_from := FCreateDateFrom, filter_create_date_to := FCreateDateTo}) -> 
%%    {{YearFrom, MonthFrom, DayFrom}, {HourFrom, MinFrom, SecFrom}} = FCreateDateFrom, %% Datetime
%%    {{YearTo, MonthTo, DayTo}, {HourTo, MinTo, SecTo}} = FCreateDateTo, %% Datetime
%%    DateFrom = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%%			 [YearFrom, MonthFrom, DayFrom, HourFrom, MinFrom, SecFrom])),
%%    DateTo = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%%			 [YearTo, MonthTo, DayTo, HourTo, MinTo, SecTo])),
%%    lists:flatten(io_lib:format("`create_date` BETWEEN STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s') AND STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s')", [DateFrom, DateTo]));
get_filter_sql_expr(#{filter_create_date_from := undefined, filter_create_date_to := undefined}) -> undefined;
get_filter_sql_expr(#{filter_create_date_from := FCreateDateFrom, filter_create_date_to := undefined}) ->
    lists:flatten(io_lib:format("`create_date` BETWEEN STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s') AND STR_TO_DATE('2999-12-31 15:59:59', '%Y-%m-%d %H:%i:%s')", [FCreateDateFrom]));
get_filter_sql_expr(#{filter_create_date_from := undefined, filter_create_date_to := FCreateDateTo}) ->
    lists:flatten(io_lib:format("`create_date` BETWEEN STR_TO_DATE('1999-12-31 15:59:59', '%Y-%m-%d %H:%i:%s') AND STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s')", [FCreateDateTo]));
get_filter_sql_expr(#{filter_create_date_from := FCreateDateFrom, filter_create_date_to := FCreateDateTo}) ->
    lists:flatten(io_lib:format("`create_date` BETWEEN STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s') AND STR_TO_DATE('~ts', '%Y-%m-%d %H:%i:%s')", [FCreateDateFrom, FCreateDateTo]));

%% Функция строит SQL условие для LIKE выборки по параметрам сервиса
get_filter_sql_expr(#{filter_log_params_like := undefined}) -> undefined;
get_filter_sql_expr(#{filter_log_params_like := FUserServiceParamsLike}) when is_list(FUserServiceParamsLike) -> 
    %% Сформировать список OR токенов
    Tokens = lists:foldr(fun({FUserServiceParamLike}, Res) ->
				 T = lists:flatten(io_lib:format("l.`params` LIKE '%~ts%'", [FUserServiceParamLike])),
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

build_query(#{
       filter_access_level := FAccessLevel,
       filter_log_id := FLogId,
       filter_log_desc_id := FLogDescId,
       filter_user_id := FUserId,
       filter_subject_ip := FSubjectIP,
       filter_create_date_from := FCreateDateFrom,
       filter_create_date_to := FCreateDateTo,
       filter_log_template_params_like := FLogParamsLike,
       filter_order_by_dir := FOrderByDir,
       filter_limit_offset := FLimitOffset,
       filter_limit_rows := FLimitRows
	     }) ->
    AccesLevelExpr = get_filter_sql_expr(#{filter_access_level => FAccessLevel}),
    LogIdExpr = get_filter_sql_expr(#{filter_log_id => FLogId}),
    LogDescIdExpr = get_filter_sql_expr(#{filter_log_desc_id => FLogDescId}),
    UserIdExpr = get_filter_sql_expr(#{filter_user_id => FUserId}),
    SubjectIpExpr = get_filter_sql_expr(#{filter_subject_ip => FSubjectIP}),
    CDateBetweenExpr = get_filter_sql_expr(#{filter_create_date_from => FCreateDateFrom, filter_create_date_to => FCreateDateTo}),
    ParamsLikeExpr = get_filter_sql_expr(#{filter_log_params_like => FLogParamsLike}),

    OrderDirSqlExpr = get_filter_sql_expr(#{filter_order_by_dir => FOrderByDir}),
    LimitSqlExpr = get_filter_sql_expr(#{filter_limit_offset => FLimitOffset, filter_limit_rows => FLimitRows}),

    io:format("DEBUG >>> log_db:build_query LimitSqlExpr = ~p~n", [LimitSqlExpr]),

    WhereTokens = [
		    {" AND ", AccesLevelExpr},
		    {" AND ", LogIdExpr},
		    {" AND ", LogDescIdExpr},
		    {" AND ", UserIdExpr},
		    {" AND ", SubjectIpExpr},
		    {" AND ", CDateBetweenExpr},
		    {" AND ", ParamsLikeExpr}
		  ],
    
    Where = sql_builder:build_expr(WhereTokens),
    
    case Where of
	[] ->
	    WhereFin = [];
	_ ->
	    WhereFin = lists:flatten(io_lib:format(" WHERE ~ts ", [Where]))
    end,

    io:format("DEBUG >>> log_db:build_query Where = ~p~n", [Where]),
    
    Sql = lists:flatten(io_lib:format(
			  " SELECT "
			  " l.log_id as 'log_id', "
			  " ld.log_desc_id as 'log_desc_id as', "
			  " l.log_template_params as 'log_template_params', "
			  " l.create_date as 'create_date', "
			  " INET6_NTOA(l.subject_ip) as 'subject_ip', "
			  " u.name as 'name', "
			  " l.user_id as 'user_id' "
			  " FROM `log` l"
			  " INNER JOIN `log_desc` ld ON l.log_desc_id=ld.log_desc_id "
			  " INNER JOIN `user` u ON l.user_id=u.user_id "
			  " ~ts ORDER BY l.create_date ~ts ~ts", [WhereFin, OrderDirSqlExpr, LimitSqlExpr])),
    Sql.


