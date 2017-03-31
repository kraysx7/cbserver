-module(partner_db).

-export([create/1, get/1, update/1, get_users_stat/1]).

-include("../include/mysql.hrl").
-include("../include/partner.hrl").


create(#{user_id := UserId, type := Type, key := Key, balance := Balance}) ->
    emysql:prepare(create_partner_stmt, <<"call `create_partner`(?, ?, ?, ?); ">>),  
    Result = emysql:execute(main_pool, create_partner_stmt, [UserId, Type, Key, Balance]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[PartnerId | _] | _] = ResultPacket#result_packet.rows,
		    partner_db:get(#{partner_id => PartnerId});
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.


get(#{partner_id := PartnerId}) ->
    emysql:prepare(get_partner_by_pid_stmt, <<"SELECT * FROM `partner` WHERE `partner_id` = ?">>),
    Result = emysql:execute(main_pool, get_partner_by_pid_stmt, [PartnerId]),
    RecResult = emysql:as_record(Result, partner, record_info(fields, partner)), 
    case RecResult of
	[Partner | _] when is_record(Partner, partner) -> {ok, Partner};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{user_id := UserId}) ->
    emysql:prepare(get_partner_by_uid_stmt, <<"SELECT * FROM `partner` WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, get_partner_by_uid_stmt, [UserId]),
    RecResult = emysql:as_record(Result, partner, record_info(fields, partner)), 
    case RecResult of
	[Partner | _] when is_record(Partner, partner) -> {ok, Partner};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{partner_key := PartnerKey}) ->
    emysql:prepare(get_partner_by_key_stmt, <<"SELECT * FROM `partner` WHERE `key` = ?">>),
    Result = emysql:execute(main_pool, get_partner_by_key_stmt, [PartnerKey]),
    RecResult = emysql:as_record(Result, partner, record_info(fields, partner)), 
    case RecResult of
	[Partner | _] when is_record(Partner, partner) -> {ok, Partner};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


update(#{field := key, value := Value, partner_id := PartnerId}) ->
    emysql:prepare(update_partner_key_stmt, <<"UPDATE `partner` SET `key` = ? WHERE `partner_id` = ?">>),
    Result = emysql:execute(main_pool, update_partner_key_stmt, [Value, PartnerId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end;

update(#{field := balance, value := Value, partner_id := PartnerId}) ->
    emysql:prepare(update_partner_balance_stmt, <<"UPDATE `partner` SET `balance` = ? WHERE `partner_id` = ?">>),
    Result = emysql:execute(main_pool, update_partner_balance_stmt, [Value, PartnerId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end.



%%%===================================================================
%%% STATISTIC FUNCTIONS
%%%===================================================================

get_users_stat(PartnerId) ->
    io:format("partner_db:get_users_stat (partner_id=~p  from=~p  to=~p~n)", [PartnerId, null, null]),
    
    emysql:prepare(get_partner_users_stat_stmt,
		   <<"SELECT u.name,u.reg_date
FROM `user` u WHERE u.partner_id = ?  ORDER BY u.`reg_date` DESC">>),
    
    Result = emysql:execute(main_pool, get_partner_users_stat_stmt, [PartnerId]),

    case Result of
	Result when is_record(Result, result_packet) -> 
	    FinRows = lists:foldl(fun(Row, Rows) ->  
					  [UserName, RegDate] = Row,
					  A = [{name, UserName}, {reg_date, RegDate}],
					  lists:append([A], Rows)
				  end, [], Result#result_packet.rows),
	    {ok, FinRows};
	_  -> {error, db_error}
    end.




