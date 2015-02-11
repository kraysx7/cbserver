-module(db_api).

-include("../include/user.hrl").
-include("../include/user_service.hrl").
-include("../include/service.hrl").
-include("../include/order.hrl").
-include("../include/node.hrl").

%%%===================================================================
%%% EXPORT BLOCK. 
%%%===================================================================

%% Export functions for table `user`
-export([get_user/1, get_user/2]).
% -export([update_balance/3, create_sb_tansaction/7]).

%% Export functions for table `order`
-export([create_order/8, update_order_status/2, get_new_orders/0]).

%% Export functions for table `service`
-export([get_service/1, get_services/1]).

%% Export functions for table `user_service`
-export([get_user_services/1, get_user_services/2]).
-export([add_user_service/6, update_user_service_status/2, get_expire_services/0]).


%% Export functions for table `nodes`
-export([get_enabled_nodes/0]).


%%%===================================================================
%%% DEFINE & RECORD BLOCK
%%%===================================================================

-record(result_packet, {seq_num, field_list, rows, extra}).

-record(ok_packet, {seq_num, affected_rows, insert_id, status, warning_count, msg}).

-record(error_packet, {seq_num, code, msg}).

%%%===================================================================
%%% API
%%%===================================================================


%%%================= Функции работы с пользователями =================

get_user(UserId) ->
    %% Получить информацию о клиенте мерчанта
    emysql:prepare(get_user_stmt, <<"SELECT * FROM `user` WHERE `user_id` = ?">>),
    Result = emysql:execute(main_pool, get_user_stmt, [UserId]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 

    case RecResult of
	[User | _] when is_record(User, user) -> {ok, User};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


get_user(UserName, Password) ->
    %% Получить информацию о клиенте мерчанта
    emysql:prepare(get_user_stmt, <<"SELECT * FROM `user` WHERE `name` = ? AND `password` = ?">>),
    Result = emysql:execute(main_pool, get_user_stmt, [UserName, Password]),
    RecResult = emysql:as_record(Result, user, record_info(fields, user)), 

    case RecResult of
	[User | _] when is_record(User, user) ->
	    case User#user.ban of
		0 ->                        {ok, User};
		BanFlag when BanFlag > 0 -> {error, banned}
	    end;
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.

%%%================= Функции работы с сервисами =================


get_service(ServiceId) ->
    emysql:prepare(get_service_stmt, <<"SELECT * FROM `service` WHERE `service_id` = ?">>),
    Result = emysql:execute(main_pool, get_service_stmt, [ServiceId]),
    RecResult = emysql:as_record(Result, service, record_info(fields, service)),
    case RecResult of
	[Service | _] when is_record(Service, service) ->
	    {ok, decode_params(Service)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


get_services(Type) ->
    emysql:prepare(get_services_stmt, <<"SELECT * FROM `service` WHERE `type` = ?">>),
    Result = emysql:execute(main_pool, get_services_stmt, [Type]),
    RecResult = emysql:as_record(Result, service, record_info(fields, service)),
    case RecResult of
	[Service | _] when is_record(Service, service) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


get_user_services({user_id, UserId}) ->
    emysql:prepare(get_user_services_stmt, 
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE us.`user_id` = ?">>),
    Result = emysql:execute(main_pool, get_user_services_stmt, [UserId]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)),
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};	
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get_user_services({status, Status}) ->
    emysql:prepare(get_new_user_services_stmt,
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE us.`status` = ?">>),
    Result = emysql:execute(main_pool, get_new_user_services_stmt, [Status]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)), 

    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


get_user_services(UserId, Type) ->
    emysql:prepare(get_user_services_stmt,
		   <<"SELECT us.*,s.type,s.name FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE `user_id` = ? AND `s.type` = ? AND `status` > 0">>),
    Result = emysql:execute(main_pool, get_user_services_stmt, [UserId, Type]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)), 
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, decode_params(RecResult)};		
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


add_user_service(UserId, ServiceId, NodeId, Params, EndDate, Status) ->
    emysql:prepare(add_user_service_stmt, <<"call `add_user_service`(?, ?, ?, ?, ?, ?); ">>),  
    Result = emysql:execute(main_pool, add_user_service_stmt, [UserId, ServiceId, NodeId, Params, EndDate, Status]),
    case Result of
	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
	    if
		 length(ResultPacket#result_packet.rows) > 0 ->
		     [[UserServiceId | _] | _] = ResultPacket#result_packet.rows,
		     {ok, UserServiceId};
		 true -> {error, db_error}
	     end;
	_ -> {error, db_error}
     end.


update_user_service_status(UserServiceId, Status) ->
    emysql:prepare(update_user_service_status_stmt, <<"UPDATE `user_service` SET `status` = ? WHERE `user_service_id` = ?">>),
    Result = emysql:execute(main_pool, update_user_service_status_stmt, [Status, UserServiceId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) ->
	    {ok, 0};
	_ -> {error, db_error}
    end.


%% функция возвращает список просроченных сервисов (требующих отключения)
get_expire_services() ->
    emysql:prepare(get_expire_services_stmt, 
		   <<"SELECT us.*,s.type FROM `user_service` us
                      LEFT JOIN `service` s ON us.service_id = s.service_id
                      WHERE (`end_date` BETWEEN ? AND ? ) AND us.`status` > 0">>),
    
    LocalDateTime = calendar:local_time(),
    StartDateTime = {{1970,1,1},{0,0,0}},

    Result = emysql:execute(main_pool, get_expire_services_stmt, [datetime_to_mysql_format(StartDateTime),datetime_to_mysql_format(LocalDateTime)]),
    RecResult = emysql:as_record(Result, user_service, record_info(fields, user_service)), 
    
    case RecResult of
	[UserService | _] when is_record(UserService, user_service) ->
	    {ok, RecResult};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.

%%%================= Функции работы с заказами =================

create_order(UserId, ServiceId, Params, Period, Status, ProcessingMode, CreateDate, CloseDate) ->
    emysql:prepare(create_order_stmt, <<"call `create_order`(?, ?, ?, ?, ?, ?, ?, ?); ">>),
    
    Result = emysql:execute(main_pool, create_order_stmt, [UserId, ServiceId, Params, Period, Status, ProcessingMode, CreateDate, CloseDate]),
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

    
update_order_status(OrderId, Status) ->
    emysql:prepare(update_order_status_stmt, <<"UPDATE `order` SET `status` = ? WHERE `order_id` = ?">>),
    Result = emysql:execute(main_pool, update_order_status_stmt, [Status, OrderId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) ->
	    {ok, 0};
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


%%%================= Функции работы с узлами =================

get_enabled_nodes() ->
    emysql:prepare(get_nodes_stmt, <<"SELECT * FROM `node` WHERE `status` = 0 ">>),
    Result = emysql:execute(main_pool, get_nodes_stmt, []),
    RecResult = emysql:as_record(Result, node, record_info(fields, node)), 
    case RecResult of
	[] -> {ok, []};
	[Node | _] when is_record(Node, node) -> {ok, RecResult};
	_  -> {error, undefined}
    end.



%% -----------------  Недоделаное ----------------- 






%create_tansaction(UserId, Summa, NewBalance, Status, DateOpen, DateClose) ->
%    gen_server:call(?SERVER, {create_transaction, UserId, Summa, NewBalance, Status, DateOpen, DateClose}).


%update_balance(MerchantId, CustomerId, Summa) ->
%    gen_server:call(?SERVER, {update_balance, MerchantId, CustomerId, Summa}).


%% {ok, T, _} = erl_scan:string("{1,2,[hello]}."), erl_parse:parse_term(T).


%% handle_call({create_cb_transaction, UserId, Summa, NewBalance, Status, DateOpen, DateClose}, _From, State) ->
%%     %% создаём транзакцию
%%     emysql:prepare(call_create_sb_transaction, <<"call `create_sb_transaction`(?, ?, ?, ?, ?, ?, ?);">>),
%%     Result = emysql:execute(main_pool, call_create_sb_transaction, [MerchantId, CustomerId, Summa, NewBalance, Status, DateOpen, DateClose]),

%%     case Result of
%% 	[ResultPacket, _] when is_record(ResultPacket, result_packet) ->
%% 	    if
%% 		length(ResultPacket#result_packet.rows) > 0 ->
%% 		    [[TransactionId | _] | _] = ResultPacket#result_packet.rows,
%% 		    {reply, {ok, TransactionId},  State};
%% 		true -> {reply, {error, db_error}, State}
%% 	    end;
%% 	_ -> {reply, {error, db_error}, State}
%%     end;


%% %% НЕДОДЕЛАНО !!!!
%% handle_call({update_balance, UserId, Summa}, _From, State) ->
%%     %% получаем информацию о пользователе
%%     {reply, CustomerRes, State} = handle_call({get_customer, MerchantId, CustomerId}, _From, State),

%%     case CustomerRes of
%% 	{ok, Customer} -> 
%% 	    %% проверить возможность обновления и обновить баланс :
%% 	    NewBalance = Customer#customer.balance + Summa,
%% 	    if 
%% 		%% можно обновлять :
%% 		NewBalance >= 0 ->
%% 		    %% т.к. call в gen_server - атомарная операция (все сообщения выстраиваются в очередь) ,
%% 		    %% то транзакция обновления баланса создаётся уже завершённой

%% 		    DateOpen = datetime_to_mysql_format(calendar:local_time()),
%% 		    DateClose= DateOpen,
%% 		    Status = 1,

%% 		    %% создаём транзакцию :
%% 		    {reply, TrCreateRes, State} = handle_call({create_sb_transaction, MerchantId, CustomerId, Summa, NewBalance, Status, DateOpen, DateClose}, _From, State),
		    
%% 		    case TrCreateRes of
%% 			{ok, TransactionId} -> 
%% 			    %% обновляем баланс
%% 			    emysql:prepare(update_balance_stmt, <<"UPDATE `sb_customer` SET `balance` = ? WHERE `merchant_id` = ? AND `customer_id` = ?">>),
%% 			    Result = emysql:execute(main_pool, update_balance_stmt, [NewBalance, MerchantId, CustomerId]),

%% 			    case Result of
%% 				OkPacket when is_record(OkPacket, ok_packet) ->
%% 				    {reply, {ok, {TransactionId, Customer#customer{balance = NewBalance}}},  State};
%% 				_  -> {reply, {error, db_error}, State}
%% 			    end;
%% 			{error, Error} -> {reply, {error, Error},     State};
%% 			_              -> {reply, {error, undefined}, State}
%% 		    end; % /case
%% 		true -> {reply, {error, low_balance},  State}
%% 	    end; % /if
%% 	{error, Error} -> {reply, {error, Error},     State};
%% 	_              -> {reply, {error, undefined}, State}
%%     end; % /case .


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
    Record#order{params=TermParams};

decode_params(Record) when is_record(Record, service) ->
    TermParams = commons:decode_service_params(Record#service.params),
    %% модифицируем сруктуру сервиса
    Record#service{params=TermParams};

decode_params(Record) when is_record(Record, user_service) ->
    TermParams = commons:decode_service_params(Record#user_service.params),
    %% модифицируем сруктуру сервиса
    Record#user_service{params=TermParams}.


%%'2008-08-14 00:00:00'
%%'2008-08-23 23:59:59'
datetime_to_mysql_format({{Year, Month, Day}, {H, M, S}}) ->
    lists:flatten(io_lib:fwrite("~p-~p-~p ~p:~p:~p", [Year, Month, Day, H, M, S])). 

unixTimeToDateTime(Seconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   calendar:gregorian_seconds_to_datetime(BaseDate + Seconds).
