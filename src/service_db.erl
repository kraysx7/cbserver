-module(service_db).

-include("../include/mysql.hrl").
-include("../include/user_service.hrl").
-include("../include/service.hrl").
-include("../include/node.hrl").

%%%===================================================================
%%% EXPORT BLOCK. 
%%%===================================================================

%% Export functions for table `service`
-export([get/1, get_services/1]).

%% Export functions for table `nodes`
-export([get_enabled_nodes/0]).



get(#{service_id := ServiceId, currency_number := CurrencyNumber}) ->
    emysql:prepare(get_service_by_id_and_cur_stmt, <<"SELECT s.service_id,name,type,params,sc.cost,period,status FROM `service` s INNER JOIN `service_cost` sc ON s.service_id = sc.service_id WHERE s.`service_id` = ? AND sc.`currency_number` = ?">>),
    Result = emysql:execute(main_pool, get_service_by_id_and_cur_stmt, [ServiceId, CurrencyNumber]),
    RecResult = emysql:as_record(Result, service, record_info(fields, service)),
    case RecResult of
	[Service | _] when is_record(Service, service) ->
	    {ok, decode_params(Service)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{service_id := ServiceId}) ->
    emysql:prepare(get_service_stmt, <<"SELECT * FROM `service` WHERE `service_id` = ?">>),
    Result = emysql:execute(main_pool, get_service_stmt, [ServiceId]),
    RecResult = emysql:as_record(Result, service, record_info(fields, service)),
    case RecResult of
	[Service | _] when is_record(Service, service) ->
	    {ok, decode_params(Service)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{type := ServiceType}) ->
    emysql:prepare(get_services_by_type_stmt, <<"SELECT * FROM `service` WHERE `type` = ?">>),
    Result = emysql:execute(main_pool, get_services_by_type_stmt, [ServiceType]),
    RecResult = emysql:as_record(Result, service, record_info(fields, service)),
    case RecResult of
	[Service | _] when is_record(Service, service) ->
	    {ok, decode_params(RecResult)};
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

decode_params(Record) when is_record(Record, service) ->
    TermParams = commons:decode_service_params(Record#service.params),
    %% модифицируем сруктуру сервиса
    Record#service{params=TermParams}.




%%'2008-08-14 00:00:00'
%%'2008-08-23 23:59:59'
datetime_to_mysql_format({{Year, Month, Day}, {H, M, S}}) ->
    lists:flatten(io_lib:fwrite("~p-~p-~p ~p:~p:~p", [Year, Month, Day, H, M, S])). 

unixTimeToDateTime(Seconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   calendar:gregorian_seconds_to_datetime(BaseDate + Seconds).
