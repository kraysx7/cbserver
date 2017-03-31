-module(main_app).
-behaviour(application).

-include("../include/user_promocode.hrl").

-include("../include/user_service.hrl").
-include("../include/user_service_param.hrl").

-include("../include/transaction.hrl").

-include("../include/rate.hrl").

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:start(sasl),
    application:start(crypto),
    application:start(iconv),

    %% Создаём схему БД mnesia
    MnesiaNodes = [node()],
    case mnesia:create_schema(MnesiaNodes) of
	ok -> 
	    io:format("DEBUG>>> main_app:init_mnesia ok!~n");
	{error, {_,{already_exists, _}}} ->
	    io:format("DEBUG>>> main_app:init_mnesia already_exists!~n")
    end,

    %% Запускаем mnesia
    application:start(mnesia),
    application:ensure_all_started(mnesia),

    %% Инициализируем mnesia
    init_mnesia(),

    %% Запускаем emysql
    application:start(emysql),
    application:ensure_all_started(emysql),

    emysql:add_pool(main_pool, [
				{size,100},
				{user,config:get(database_login)}, {password, config:get(database_pass)},
				{host,config:get(database_host)}, {port, config:get(database_port)}, {database, config:get(database_name)}, {encoding,utf8}
			       ]),


    %% Запускаем worker_pool (inaka)
    %%application:start(worker_pool),
    %%application:ensure_all_started(worker_pool),
    wpool:start(),
    
    %% Пул процессов для запросов загрузки блока данных из БД в кэш user_service
    wpool:start_sup_pool(us_load_data_block_pool, [{workers, 8}]),

    %% Пул процессов для записи в кэш user_service
    wpool:start_sup_pool(us_cache_write_pool, [{workers, 512}]),

    %% Пул процессов для чтения из кэша user_service
    wpool:start_sup_pool(us_cache_read_pool, [{workers, 512}]),

    %% Пул процессов для запросов загрузки блока данных из БД в кэш user_service
    wpool:start_sup_pool(u_cache_pool, [{workers, 200}]),

    %% запускаем HTTP(S) клиент
    shotgun:start(),

    main_sup:start_link().

stop(_State) ->
    ok.




init_mnesia() ->
    %% Создаём схему кэш таблички user_service
    mnesia:create_table(user_service_cache,
     			[{attributes, record_info(fields, user_service)},
     			 {record_name, user_service},
     			 {type, set}]),
    
    mnesia:create_table(user_service_params_cache,
     			[{attributes, record_info(fields, user_service_param)},
     			 {record_name, user_service_param},
     			 {type, bag}]),
    
    mnesia:wait_for_tables([user_service_cache,
			    user_service_params_cache], 5000),
    
    %% Создаём схему кэш таблички transaction
    mnesia:create_table(transaction_cache,
     			[{attributes, record_info(fields, transaction)},
     			 {record_name, transaction},
     			 {type, set}]),    

    %% Создаём схему кэш таблички cbrates (текущие(daily) котировки центрального банка)
    mnesia:create_table(cbrates,
     			[{attributes, record_info(fields, rate)},
     			 {record_name, rate},
     			 {type, set}]),
    
    mnesia:wait_for_tables([cbrates], 5000),

    %% Создаём схему кэш таблички user_promocode
    mnesia:create_table(user_promocode_cache,
     			[{attributes, record_info(fields, user_promocode)},
     			 {record_name, user_promocode},
     			 {type, set}]),
    
    mnesia:wait_for_tables([user_promocode_cache], 5000),

    %% CreateDate1 = calendar:datetime_to_gregorian_seconds({{2015, 1, 1}, {0, 0, 0}}),
    %% CreateDate2 = calendar:datetime_to_gregorian_seconds({{2016, 5, 1}, {0, 0, 0}}),
    %% CreateDate3 = calendar:datetime_to_gregorian_seconds({{2016, 2, 1}, {0, 0, 0}}),
    %% CreateDate4 = calendar:datetime_to_gregorian_seconds({{2016, 1, 1}, {0, 0, 0}}),

    %% user_service:insert_cache(#user_service{user_service_id=45, type=3001, create_date=CreateDate1, status=1}),
    %% user_service:insert_cache(#user_service{user_service_id=46, type=3002, create_date=CreateDate1, status=6}),
    %% user_service:insert_cache(#user_service{user_service_id=47, type=3001, create_date=CreateDate3, status=1}),
    %% user_service:insert_cache(#user_service{user_service_id=48, type=3002, create_date=CreateDate2, status=6}),
    %% user_service:insert_cache(#user_service{user_service_id=49, type=3003, create_date=CreateDate4, status=1}),

    io:format("DEBUG>>> main_app:init_mnesia create tables complite!~n"),
    ok.
