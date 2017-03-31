-module(user_service_cache_cntl).
-behaviour(gen_server).

-include("../include/user.hrl").
-include("../include/user_service.hrl").
-include("../include/user_service_param.hrl").

-include_lib("stdlib/include/qlc.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% stuff exports
-export([load_data_block/1, update_us_user_name/2]).


-define(START_SYNC_TIMER, 3000). % start sync after 3 seconds
-define(SERVER, ?MODULE).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    io:format("DEBUG>> user_service_cache_cntl:init args:~p~n", [Args]),
    %% Start start sync after timer
%%    erlang:send_after(?START_SYNC_TIMER, self(), start_sync),
    {ok, #state{}}.


handle_info(start_sync, State) ->
    %% Получить из БД общее количество записей в таблице user_service
    {ok, FullCount} = user_service_db:get(#{mode => full_count}),
    io:format("DEBUG>>> user_service_cache_cntl:handle_info(start_sync) full count ~p~n", [FullCount]),

    BlockSize = 10000,
    load_data(#{offset => 0, bs => BlockSize, full_count => FullCount}),

    io:format("DEBUG>>> user_service_cache_cntl:handle_info good buy!~n"),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% INTERNAL FUNCTIONS

load_data(#{offset := Offset, bs := _BlockSize, full_count := FullCount})
  when Offset >= FullCount -> ok;

load_data(#{offset := Offset, bs := BlockSize, full_count := FullCount})
  when Offset < FullCount ->
    M = user_service_cache_cntl,
    F = load_data_block,
    A = [#{offset => Offset, bs => BlockSize}],
    wpool:call(us_load_data_block_pool, {M, F, A}, available_worker, 20000),
    %% Засыпаем на 30 секунд, даём отдышатся mysql
    timer:sleep(40000),
    load_data(#{offset => Offset+BlockSize, bs => BlockSize, full_count => FullCount}).

load_data_block(#{offset := Offset, bs := BlockSize}) ->
    io:format("DEBUG>>> user_service_cache_cntl:load_data_block ~p ___ ~p ... ", [Offset, Offset+BlockSize]),
    {ok, Res} = user_service_db:get(#{limit_offset => Offset, limit_rows => BlockSize}),
    lists:foreach(fun(UserService) ->


			  %%io:format("DEBUG>>> user_service_cache_cntl:load_data_block user_service_id: ~p~n", [UserService#user_service.user_service_id]),


			  UserServiceId = UserService#user_service.user_service_id,
			  
			  %%io:format("DEBUG>>> user_service_cache_cntl:load_data_block UserService#create_date: ~p~n", [UserService#user_service.create_date]),

			  %% Преобразовать datetime в timestamp(UTC) (так проще делать выборку)
			  FCreateTimestamp = case UserService#user_service.create_date of
						     undefined -> undefined;
						     {datetime, {{_,_,_},{_,_,_}} = DT1} ->
							 calendar:datetime_to_gregorian_seconds(DT1)
						 end,
			  FEndTimestamp = case UserService#user_service.end_date of
						   undefined -> undefined;
						   {datetime, {{_,_,_},{_,_,_}} = DT2} ->
						       calendar:datetime_to_gregorian_seconds(DT2)
					       end,
			  
			  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
			  %% Текущие фиксы параметров заказов в бд

			  Params = UserService#user_service.params,

			  %% Сформировать новые параметры

			  %% Получить новые параметры с ценой
			  %%NP1 = Params,
			  NP1 = case proplists:get_value(price, Params) of
			  	    %% Если цена вообще не была установлена, устанавливаем цену на основе грейда
			  	    undefined ->
			  		Rarity1 = proplists:get_value(rarity, Params, undefined),
					Rarity = case Rarity1 of
						   undefined -> proplists:get_value(type, Params, undefined);
						   Rarity1 -> Rarity1
					       end,
			  		NewPrice = case Rarity of
			  			       knife -> 1500+random:uniform(5000-1500);
			  			       covert -> 150+random:uniform(500-150);
			  			       classified -> 70+random:uniform(300-70);
			  			       restricted -> 30+random:uniform(100-30);
			  			       _ -> undefined
			  			   end,
			  		case NewPrice of
			  		    undefined -> Params;
			  		    NewPrice ->
			  			P1 = lists:flatten(proplists:delete(price, Params), [{price, NewPrice}]),
						
			  			UserServiceId = UserService#user_service.user_service_id,
			  			%% io:format("DEBUG>>> price undefined (us: ~p) : new value: ~p ...", [UserServiceId, NewPrice]),
						
			  			%% Сохранить в БД
			  			A1 = [#{user_service_id => UserServiceId, params => P1}],
			  			wpool:cast(us_cache_read_pool, {user_service, update, A1}),
						
			  			%% io:format("ok!~n"),
			  			P1
			  		end;
			  	    CurPrice ->
			  		%% Если цена на месте, устанавливаем цену на основе исключений по имени скина
			  		ItemName = proplists:get_value(item_name, Params),
			  		%% io:format("DEBUG>>> item_name ~p~n", [ItemName]),
					
			  		{R1,R2,R3} = now(),
			  		random:seed(R1, R2, R3),
					
			  		NewPrice = case {ItemName, CurPrice} of
			  			       {_, 0} -> 
			  				   Rarity = proplists:get_value(rarity, Params),
			  				   case Rarity of
			  				       knife -> 1500+random:uniform(5000-1500);
			  				       covert -> 150+random:uniform(500-150);
			  				       classified -> 70+random:uniform(300-70);
			  				       restricted -> 30+random:uniform(100-30);
			  				       _ -> undefined
			  				   end;
			  			       %{"M4A4 | Poseidon",_} -> 7000+random:uniform(8000-7000);
			  			       %{"M4A4 | Howl",_} -> 30000+random:uniform(35000-30000);
			  			       %{"AWP | Medusa",_} -> 30000+random:uniform(35000-30000);
			  			       {"AWP | Dragon Lore",_} -> 
						        	   io:format("DEBUG>>> item_name AWP | Dragon Lore~n"),
						        	   20000+random:uniform(35000-20000);
			  			       _ -> CurPrice
			  			   end,
					
			  		case NewPrice of
			  		    undefined -> Params;
			  		    CurPrice -> Params;
			  		    NewPrice ->
			  			UserServiceId = UserService#user_service.user_service_id,
			  			P2 = lists:flatten(proplists:delete(price, Params), [{price, NewPrice}]), 
						
			  			%% Сохранить в БД
			  			A2 = [#{user_service_id => UserServiceId, params => P2}],
			  			wpool:cast(us_cache_read_pool, {user_service, update, A2}),
			  			P2
			  		end
			  	end,
			  
			  %% Перенести type в rarity
			  NP2 = case proplists:get_value(rarity, NP1, undefined) of
				    undefined ->
					FixRarity = proplists:get_value(type, NP1, undefined),
					case FixRarity of
					    undefined -> NP1;
					    FixRarity -> 
						P3 = lists:flatten(proplists:delete(type, NP1), [{rarity, FixRarity}]),
			  			%% Сохранить в БД
			  			A3 = [#{user_service_id => UserServiceId, params => P3}],
			  			wpool:cast(us_cache_read_pool, {user_service, update, A3}),
			  			P3
					end;
				    OriginalRarity -> NP1
				end,
			  
			  %% Заносим в кэш
			  
			  FinUserService = UserService#user_service{create_date=FCreateTimestamp, end_date=FEndTimestamp, params=NP2},

			  
			  wpool:cast(us_cache_write_pool, {user_service, insert_cache, [FinUserService]})
			  %%io:format("ok!~n")
			  %% %% Пройтись по параметрам тек. сервиса
			  %% lists:foreach(fun({Key, Value}) -> 
			  %% 			US_Param = #user_service_param{user_service_id=UserServiceId, param_key=Key, param_value=Value},
			  %% 			Args = #{mode => durty, param => US_Param},
			  %% 			wpool:cast(us_cache_write_pool, {user_service, insert_param_cache, [Args]})
			  %% 		end, UserService#user_service.params)
		  end, Res),
    
    io:format("loaded!~n"), ok.



update_us_user_name(UserService, Params) ->
    UserServiceId = UserService#user_service.user_service_id,
    UserId = UserService#user_service.user_id,
    %%io:format("DEBUG>>> us_cache_cntl:load_data_block, user name not present! set..."),
    
    %%timer:sleep(400),
    URes = user:get(#{user_id => UserId}),
    case URes of
	{ok, User} ->
	    P = lists:flatten(proplists:delete(user_name, Params), [{user_name, User#user.name}]), 
	    
	    %% Сохранить в БД
	    user_service:update(#{user_service_id => UserServiceId, params => P});
	    %%io:format("ok!~n");
	_ -> ok%%io:format("error!~n")
    end.


