-module(main_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:start(sasl),
    application:start(crypto),

    application:start(mnesia),

    application:start(emysql),

    emysql:add_pool(main_pool, [
				{size,1},
				{user,config:get(database_login)}, {password, config:get(database_pass)},
				{host,config:get(database_host)}, {port, config:get(database_port)}, {database, config:get(database_name)}, {encoding,utf8}
			       ]),

    main_sup:start_link().

stop(_State) ->
    ok.
