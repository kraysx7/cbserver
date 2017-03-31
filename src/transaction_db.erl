-module(transaction_db).

-include("../include/mysql.hrl").
-include("../include/transaction.hrl").

%% Export functions for table `order`
-export([create/1, close/1, get/1, update/1, set_status/2, get_first_payments/0]).

%%%================= Функции для работы с заказами в БД =================
create(#{type := Type,
	 params := Params,
	 user_id := UserId,
	 currency_alpha := CurrencyAlpha,
	 currency_number := CurrencyNumber,
	 cost := Cost,
	 new_balance := NewBalance,
	 status := Status,
	 create_date := CreateDate,
	 close_date := CloseDate}) ->
    emysql:prepare(proc_create_tr_stmt, <<"call `create_transaction`(?, ?, ?, ?, ?, ?, ?, ?, ?, ?); ">>),
    
    Result = emysql:execute(main_pool, proc_create_tr_stmt, [Type, Params, UserId, CurrencyAlpha, CurrencyNumber, Cost, NewBalance, Status, CreateDate, CloseDate]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[TrId | _] | _] = ResultPacket#result_packet.rows,
		    {ok, TrId};
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.

close(#{tr_id := TrId, new_balance := NewBalance, status := Status, close_date := CloseDate}) ->
    emysql:prepare(close_tr_stmt, <<"UPDATE `transaction` SET `new_balance` = ?, `status` = ?, `close_date` = ? WHERE `transaction_id` = ?">>),
    Result = emysql:execute(main_pool, close_tr_stmt, [NewBalance, Status, CloseDate, TrId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end.

get(#{user_id := UserId, type := Type, currency_number := CurrencyNumber, status := Status, close_date_from := CloseDateFrom}) ->
    emysql:prepare(get_trs_full_1_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ? AND `type` = ? AND `currency_number` = ? AND `status` = ? AND `close_date` > ?">>),
    Result = emysql:execute(main_pool, get_trs_full_1_stmt, [UserId, Type, CurrencyNumber, Status, CloseDateFrom]),

    RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
    case RecResult of
	[Tr | _] when is_record(Tr, transaction) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{user_id := UserId, type := [S1,S2,S3]}) ->
    emysql:prepare(get_trs_by_user_and_type_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ? AND (type = ? OR type = ? OR type = ?)">>),
    Result = emysql:execute(main_pool, get_trs_by_user_and_type_stmt, [UserId, S1, S2, S3]),

    RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
    case RecResult of
	[Tr | _] when is_record(Tr, transaction) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{type := Type, close_date := CloseDate}) ->
    emysql:prepare(get_trs_by_close_date_stmt, <<"SELECT * FROM `transaction` WHERE `type` = ? AND `close_date` = ?">>),
    Result = emysql:execute(main_pool, get_trs_by_close_date_stmt, [Type, CloseDate]),

    RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
    case RecResult of
	[Tr | _] when is_record(Tr, transaction) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{tr_id := TrId}) ->
    emysql:prepare(get_trs_by_tr_id_stmt, <<"SELECT * FROM `transaction` WHERE `transaction_id` = ?">>),
    Result = emysql:execute(main_pool, get_trs_by_tr_id_stmt, [TrId]),

    RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
    case RecResult of
	[Tr | _] when is_record(Tr, transaction) ->
	    {ok, decode_params(Tr)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{user_id := UserId}) ->
    emysql:prepare(get_trs_by_user_id_stmt, <<"SELECT * FROM `transaction` WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, get_trs_by_user_id_stmt, [UserId]),

    RecResult = emysql:as_record(Result, transaction, record_info(fields, transaction)), 
    case RecResult of
	[Tr | _] when is_record(Tr, transaction) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


update(#{transaction_id := TrId, params := Params}) ->
    emysql:prepare(update_tr_params_stmt, <<"UPDATE `transaction` SET `params` = ? WHERE `transaction_id` = ?">>),
    Result = emysql:execute(main_pool, update_tr_params_stmt, [Params, TrId]),
    io:format("DEBUG>>> transaction_db:update params res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;

update(#{transaction_id := TrId, cost := Cost}) ->
    emysql:prepare(update_tr_cost_stmt, <<"UPDATE `transaction` SET `cost` = ? WHERE `transaction_id` = ?">>),
    Result = emysql:execute(main_pool, update_tr_cost_stmt, [Cost, TrId]),
    io:format("DEBUG>>> transaction_db:update cost res ~p~n", [Result]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end.



get_first_payments() ->
    emysql:prepare(get_first_payments_stmt, <<"SELECT u.name,tr.cost,tr.close_date FROM `transaction` tr INNER JOIN `user` u ON u.user_id=tr.user_id WHERE tr.`type`=10 AND `close_date` IS NOT NULL GROUP BY tr.`user_id` ORDER BY `close_date` DESC">>),
    Result = emysql:execute(main_pool, get_first_payments_stmt, []),

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


set_status(TrId, Status) ->
    emysql:prepare(set_tr_status_stmt, <<"UPDATE `transaction` SET `status` = ? WHERE `transaction_id` = ?">>),
    Result = emysql:execute(main_pool, set_tr_status_stmt, [Status, TrId]),
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

decode_params(Record) when is_record(Record, transaction) ->
    TermParams = commons:decode_service_params(Record#transaction.params),
    Record#transaction{params=TermParams}.

