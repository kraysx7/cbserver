-module(user_promocode_db).

-export([create/1, get/1, update/1]).

-include("include/mysql.hrl").
-include("include/user_promocode.hrl").

create(#{promocode_id := PromocodeId, user_id := UserId, params := Params, activate_date_dt := ActivateDate, activate_ip := ActivateIpInt, activate_ip_alpha := ActivateIpAlpha, status := Status}) ->
    emysql:prepare(create_user_promocode_stmt, <<"call `create_user_promocode`(?, ?, ?, ?, ?, ?, ?); ">>),
    Result = emysql:execute(main_pool, create_user_promocode_stmt, [PromocodeId, UserId, Params, ActivateDate, ActivateIpInt, ActivateIpAlpha, Status]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[UserPromocodeId | _] | _] = ResultPacket#result_packet.rows,
		    user_promocode_db:get(#{user_promocode_id => UserPromocodeId});
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.

get(#{promocode_id := PromocodeId, user_id := UserId, activate_date_from := ActivateDateFrom}) ->
    emysql:prepare(get_user_promocode_var1_stmt, <<"SELECT * FROM `user_promocode` WHERE `promocode_id` = ? AND `user_id` = ? AND `activate_date` > ?">>),
    Result = emysql:execute(main_pool, get_user_promocode_var1_stmt, [PromocodeId, UserId, ActivateDateFrom]),
    RecResult = emysql:as_record(Result, user_promocode, record_info(fields, user_promocode)), 
    case RecResult of
	[Promocode | _] when is_record(Promocode, user_promocode) -> {ok, Promocode};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{user_promocode_id := UserPromocodeId}) ->
    emysql:prepare(get_user_promocode_by_id_stmt, <<"SELECT * FROM `user_promocode` WHERE `user_promocode_id` = ?">>),
    Result = emysql:execute(main_pool, get_user_promocode_by_id_stmt, [UserPromocodeId]),
    RecResult = emysql:as_record(Result, user_promocode, record_info(fields, user_promocode)), 
    case RecResult of
	[Promocode | _] when is_record(Promocode, user_promocode) -> {ok, Promocode};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


update(#{user_promocode_id := UserPromocodeId, activate_date := ActivateDate, status := Status}) ->
    emysql:prepare(update_user_promocode_status_stmt, <<"UPDATE `user_promocode` SET `activate_date` = ?, `status` = ? WHERE `user_promocode_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_promocode_status_stmt, [ActivateDate, Status, UserPromocodeId]),
    RecResult = emysql:as_record(Result, user_promocode, record_info(fields, user_promocode)), 
    case RecResult of
	[UserPromocode | _] when is_record(UserPromocode, user_promocode) -> {ok, UserPromocode};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.
