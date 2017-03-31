-module(order_db).

-include("../include/mysql.hrl").
-include("../include/order.hrl").

%% Export functions for table `order`
-export([create/1, get/1, update/1, set_status/2, get_new_orders/0]).

%%%================= Функции для работы с заказами в БД =================

create(#{
	  user_id := UserId,
	  service_id := ServiceId,
	  params := Params,
	  period := Period,
	  status := Status,
	  processing_mode := ProcessingMode,
	  create_date := CreateDate,
	  close_date := CloseDate
	}) ->
    emysql:prepare(create_order_stmt, <<"call `create_order`(?, ?, ?, ?, ?, ?, ?, ?); ">>),
    
    Result = emysql:execute(main_pool, create_order_stmt, [UserId, ServiceId, Params, Period, Status, ProcessingMode, CreateDate, CloseDate]),
    io:format("Create oreder res : ~p~n", [Result]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		length(ResultPacket#result_packet.rows) > 0 ->
		    [[OrderId | _] | _] = ResultPacket#result_packet.rows,
		    {ok, OrderId};
		true -> {error, db_error}
	    end;
	_ -> {error, db_error}
    end.



get(#{user_id := UserId}) ->
    emysql:prepare(get_orders_by_user_id_1_stmt, <<"SELECT * FROM `order` WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, get_orders_by_user_id_1_stmt, [UserId]),

    RecResult = emysql:as_record(Result, order, record_info(fields, order)), 
    case RecResult of
	[Order | _] when is_record(Order, order) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;



get({user_id, UserId}) ->
    emysql:prepare(get_orders_by_user_id_stmt, <<"SELECT * FROM `order` WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, get_orders_by_user_id_stmt, [UserId]),

    RecResult = emysql:as_record(Result, order, record_info(fields, order)), 
    case RecResult of
	[Order | _] when is_record(Order, order) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;



get(OrderId) ->
    emysql:prepare(get_order_stmt, <<"SELECT * FROM `order` WHERE `order_id` = ?">>),
    Result = emysql:execute(main_pool, get_order_stmt, [OrderId]),

    RecResult = emysql:as_record(Result, order, record_info(fields, order)), 
    case RecResult of
	[Order | _] when is_record(Order, order) ->
	    {ok, decode_params(Order)};		
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.




update(#{order_id := OrderId, status := Status, close_date := CloseDate}) ->
    emysql:prepare(update_order_1_stmt, <<"UPDATE `order` SET `status` = ?, `close_date` = ? WHERE `order_id` = ?">>),
    Result = emysql:execute(main_pool, update_order_1_stmt, [Status, CloseDate, OrderId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end;

update(#{order_id := OrderId, params := Params}) ->
    emysql:prepare(update_order_2_stmt, <<"UPDATE `order` SET `params` = ? WHERE `order_id` = ?">>),
    Result = emysql:execute(main_pool, update_order_2_stmt, [Params, OrderId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end.


set_status(OrderId, Status) ->
    emysql:prepare(set_order_status_stmt, <<"UPDATE `order` SET `status` = ? WHERE `order_id` = ?">>),
    Result = emysql:execute(main_pool, set_order_status_stmt, [Status, OrderId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_  -> {error, db_error}
    end.



get_new_orders() ->
    emysql:prepare(get_new_orders_stmt, 
		   <<"SELECT o.*, s.type as 'service_type' FROM `order` o
                      LEFT JOIN `service` s ON o.service_id = s.service_id
                      WHERE o.`status` = 0 AND `processing_mode` = 0 ">>),
    
    Result = emysql:execute(main_pool, get_new_orders_stmt, []),
    RecResult = emysql:as_record(Result, order, record_info(fields, order)), 
    
    case RecResult of
	[Order | _] when is_record(Order, order) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
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

decode_params(Record) when is_record(Record, order) ->
    TermParams = commons:decode_service_params(Record#order.params),
    Record#order{params=TermParams}.

