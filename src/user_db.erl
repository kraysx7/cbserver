-module(user_db).

-include("../include/mysql.hrl").
-include("../include/user.hrl").

-export([create/1, get/1, update/1]).

% -export([update_balance/3, create_sb_tansaction/7]).

%%%================= Функции работы с пользователями =================

create(#{remote_user_id := RemoteUserId,
	 remote_auth_type := RemoteAuthType,
	 user_name := UserName,

	 currency_alpha := CurrencyAlpha,
	 currency_number := CurrencyNumber,
	 country_number := CountryNumber,

	 password := Password, 
	 email := EMail,
	 params := Params} = Map) ->
    io:format("create user : ~p~n", [Map]),
    emysql:prepare(proc_create_user_stmt, <<"call `create_user`(?, ?, ?, ?, ?, ?, ?, ?, ?);">>),
    
    Result = emysql:execute(main_pool, proc_create_user_stmt, [RemoteUserId, RemoteAuthType, UserName, CurrencyAlpha, CurrencyNumber, CountryNumber, Password, EMail, Params]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[UserId | _] | _] = ResultPacket#result_packet.rows,
		    user_db:get(#{user_id => UserId});
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.




get(#{name := UserName, password := Password}) ->
    emysql:prepare(get_user_stmt, <<"SELECT * FROM `user` WHERE `name` = ? AND `password` = ?">>),
    Result = emysql:execute(main_pool, get_user_stmt, [UserName, Password]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 

    case RecResult of
	[User | _] when is_record(User, user) ->
	    case User#user.ban of
		0 ->                        {ok, decode_params(User)};
		BanFlag when BanFlag > 0 -> {error, banned}
	    end;
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{remote_user_id := RemoteUserId, password := Password}) ->
    emysql:prepare(get_user_by_rid_pass_stmt, <<"SELECT * FROM `user` WHERE `remote_user_id` = ? AND `password` = ?">>),
    Result = emysql:execute(main_pool, get_user_by_rid_pass_stmt, [RemoteUserId, Password]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 
    case RecResult of
	[User | _] when is_record(User, user) -> {ok, decode_params(User)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{user_id := UserId}) ->
    emysql:prepare(get_user_by_id_stmt, <<"SELECT * FROM `user` WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, get_user_by_id_stmt, [UserId]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 
    case RecResult of
	[User | _] when is_record(User, user) -> {ok, decode_params(User)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{remote_user_id := RemoteUserId}) ->
    emysql:prepare(get_user_by_rid_stmt, <<"SELECT * FROM `user` WHERE `remote_user_id` = ?">>),
    Result = emysql:execute(main_pool, get_user_by_rid_stmt, [RemoteUserId]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 
    case RecResult of
	[User | _] when is_record(User, user) -> {ok, decode_params(User)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{email := EMail}) ->
    %% Получить информацию о пользователе по email
    emysql:prepare(get_user_by_email_stmt, <<"SELECT * FROM `user` WHERE `email` = ?">>),
    Result = emysql:execute(main_pool, get_user_by_email_stmt, [EMail]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 
    case RecResult of
	[User | _] when is_record(User, user) -> {ok, decode_params(User)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;


get(#{role := Role}) ->
    emysql:prepare(get_user_by_role_stmt, <<"SELECT * FROM `user` WHERE `role` = ?">>),
    Result = emysql:execute(main_pool, get_user_by_role_stmt, [Role]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 
    case RecResult of
	[User | _] when is_record(User, user) -> {ok, decode_params(User)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{name := UserName}) ->
    emysql:prepare(get_user_by_name_stmt, <<"SELECT * FROM `user` WHERE `name` = ?">>),
    Result = emysql:execute(main_pool, get_user_by_name_stmt, [UserName]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 
    case RecResult of
	[User | _] when is_record(User, user) -> {ok, decode_params(User)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


update(#{user_id := UserId, currency_alpha := CurrencyAlpha, currency_number := CurrencyNumber, country_number := CountryNumber}) ->
    emysql:prepare(update_user_currency_stmt, <<"UPDATE `user` SET `currency_alpha` = ?, `currency_number` = ?, `country_number` = ? WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_currency_stmt, [CurrencyAlpha, CurrencyNumber, CountryNumber, UserId]),
    io:format("Update currency_alpha,currency_number,country_number res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;

update(#{remote_user_id := RemoteUserId, user_name := UserName}) ->
    emysql:prepare(update_user_rid_name_stmt, <<"UPDATE `user` SET `name` = ? WHERE `remote_user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_rid_name_stmt, [UserName, RemoteUserId]),
    io:format("Update user_name res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;


update(#{user_id := UserId, params := Params}) ->
    emysql:prepare(update_user_id_params_stmt, <<"UPDATE `user` SET `params` = ? WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_id_params_stmt, [Params, UserId]),
    io:format("Update params res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;


update(#{user_id := UserId, balance := Balance}) ->
    emysql:prepare(update_user_id_balance_stmt, <<"UPDATE `user` SET `balance` = ? WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_id_balance_stmt, [Balance, UserId]),
    io:format("Update balance res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;

update(#{user_id := UserId, last_ip := LastIp, last_ip_alpha := LastIpAlpha}) ->
    emysql:prepare(update_user_last_ip_stmt, <<"UPDATE `user` SET `last_ip` = ?,`last_ip_alpha` = ? WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_last_ip_stmt, [LastIp, LastIpAlpha, UserId]),
    io:format("Update last_ip res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;

update(#{user_id := UserId, partner_id := PartnerId}) ->
    emysql:prepare(update_user_id_partner_id_stmt, <<"UPDATE `user` SET `partner_id` = ? WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_id_partner_id_stmt, [PartnerId, UserId]),
    io:format("Update partner_id res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;

update(#{user_id := UserId, role := Role}) ->
    emysql:prepare(update_user_id_partner_id_stmt, <<"UPDATE `user` SET `role` = ? WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_id_partner_id_stmt, [Role, UserId]),
    io:format("Update role res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
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

decode_params(Record) when is_record(Record, user) ->
    TermParams = commons:decode_service_params(Record#user.params),
    %% модифицируем сруктуру сервиса
    Record#user{params=TermParams}.
