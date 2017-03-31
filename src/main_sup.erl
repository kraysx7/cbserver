-module(main_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(main_sup, []).

init(_Args) ->
    {ok, {{one_for_one, 100, 1},
	  [
	   {user_service_cache_cntl,      {user_service_cache_cntl,   start_link, []}, permanent, brutal_kill, worker, []},
	   {rates_srv,                    {rates_srv,                 start_link, []}, permanent, brutal_kill, worker, []}
	   %%{nodes_controller,           {nodes_controller,          start_link, []}, permanent, brutal_kill, worker, []},
	   %%{orders_controller,          {orders_controller,         start_link, []}, permanent, brutal_kill, worker, []},
	   %%{services_controller,        {services_controller,       start_link, []}, permanent, brutal_kill, worker, []}
	  ]
	 }
    }. 
