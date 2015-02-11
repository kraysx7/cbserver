-module(orders_controller).
-behaviour(gen_server).

-include("../include/user.hrl").
-include("../include/user_service.hrl").
-include("../include/service.hrl").
-include("../include/order.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECK_INTERVAL, 1000). % one second
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

init(_Args) ->
    io:format("DEBUG>> Orders controller started!~n"),
    %% Start first timer
    erlang:send_after(?CHECK_INTERVAL, self(), trigger),
    {ok, #state{}}.


handle_info(trigger, State) ->
    %% Получить из БД список необработтаных заказов
    case db_api:get_new_orders() of
	{ok, Orders} when is_list(Orders) ->
	    io:format("DEBUG>> New orders: ~p~n", [Orders]),
	    %% пройтись по списку и вызвать обработчик для каждого заказа :
	    lists:foreach(fun(Order) ->
				  %% Обработать заказ
				  process_new_order(Order)
			  end, Orders);
	{error, Reason} -> {error, Reason};
	_ -> unknown_error
    end,

    %% Start new timer
    erlang:send_after(?CHECK_INTERVAL, self(), trigger),
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

%% Функция обрабатывает новый заказ
process_new_order(Order) when is_record(Order, order) ->
    %% Пометить статус заказа в БД как 'поступивший'=1
    db_api:update_order_status(Order#order.order_id, 1),

    %% Проверить статус(=0) и режим обработки(=0)
    case {Order#order.processing_mode, Order#order.status} of
	{0,0} -> spawn(fun() -> process_new_order_(Order) end);
	_ -> error %% ошибка запроса к БД, статус и режим обработки некорректны
    end;
process_new_order(_Order) -> invalid_record.


process_new_order_(Order) ->
    %% Получить данные пользователя
    UserData = db_api:get_user(Order#order.user_id),
    
    %% Получить информацию о подключаемом сервисе
    ServiceData = db_api:get_service(Order#order.service_id),

    io:format("-----> user_data: ~p    service_data: ~p~n",[UserData, ServiceData]),

    %% Получить данные пользователя
    case {UserData, ServiceData} of
	{{ok, User}, {ok, Service}} when is_record(User, user) and
					 is_record(Service, service) ->
	    %% Проверить наличие средств для оплаты периода указанного в заказе
	    case User#user.balance - Service#service.cost of
		D when D >=0 ->
		    %% Средств хватает. Подключить сервис.
		    ParamsStr = lists:flatten(io_lib:format("~p.", [Order#order.params])),
		    NodeId = 0, %% узел-обработчик ещё не назначен
		    EndDate = null, %% дата отключения ещё неизвестна
		    Status = 0, %% требует активации
		    
		    %% Добавить сервис в базу
		    db_api:add_user_service (User#user.user_id, Order#order.service_id, NodeId, ParamsStr, EndDate, Status),
		    
		    %% Пометить заказ как 'обработанный'=2
		    db_api:update_order_status(Order#order.order_id, 2);
		_ ->
		    %% Средств не хватает 
		    %% Пометить статус заказа в БД как 'требующий обработки'=0
		    db_api:update_order_status(Order#order.order_id, 0),
		    low_balance
	    end;
	_ -> 
	    %% Добавить в лог отчёт о программной ошибке при работе с бд
	    error
    end.
