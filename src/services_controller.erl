-module(services_controller).
-behaviour(gen_server).

-include("../include/user_service.hrl").
-include("../include/node.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Типы сервисов :                             | Параметры (пример) :
%% 0x00000001 - OpenPanel hosting Fixed   | [{vhosts_count, 5}, {db_count, 5}, {hdd, 2048}]
%% 0x00000002 - OpenPanel hosting Scale   | [{vhost_cost, 10}, {db_cost, 10}, {hdd_cost, 10}]
%% 0x00000004 - cPanel hosting Fixed      | [{vhost_cost, 10}, {db_cost, 10}, {hdd_cost, 10}]

%% 0x00000100 - OpenNebula VPS/KVM Fixed  | [{cpus, 5}, {memory, 5}, {hdd, 10240}, {traffic, 1024000}]
%% 0x00000200 - OpenNebula VPS/KVM Scale  | 

%% 0x00001000 - TCP-Proxy network filter
%% 0x00002000 - Web-Proxy network filter

%% 0x01000000 - Radio (WiFi)  internet access
%% 0x02000000 - Cable (RJ-45) internet access

-define(OPENPANEL_HOSTING_FIXED, 16#00000001). %% 1
-define(OPENPANEL_HOSTING_SCALE, 16#00000002). %% 2

-define(OPENNEBULA_HOSTING_FIXED, 16#00000100). %% 256
-define(OPENNEBULA_HOSTING_SCALE, 16#00000200). %% 512



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
    io:format("DEBUG>> Services controller started!~n"),

    %% Запустить таймер, обрабатывающий неактивированные пользовательские сервисы
    erlang:send_after(?CHECK_INTERVAL, self(), activate_user_services_timer),

    %% Запустить таймер, обрабатывающий просроченные пользовательские сервисы
    erlang:send_after(?CHECK_INTERVAL, self(), disable_expires_timer),
    {ok, #state{}}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Хэндлер таймера, обрабатывающего неактивированные пользовательские сервисы
%%
handle_info(activate_user_services_timer, State) ->
    %%io:format("DEBUG>> Activate user services timer boooom ! ~n"),

    %% Получить из БД список неактивированных сервисов
    case db_api:get_user_services({status, 0}) of
	{ok, UserServices} when is_list(UserServices) ->
	    %% пройтись по списку сервисов и вызвать ф-ю активации для каждого
	    lists:foreach(fun(UserService) ->
				  activate_user_service(UserService)
			  end, UserServices);
	{error, Reason} -> {error, Reason};
	_ -> unknown_error
    end,

    %% Start new timer
    erlang:send_after(?CHECK_INTERVAL, self(), activate_user_services_timer),
    {noreply, State};




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Хэндлер таймера, обрабатывающего просроченные пользовательские сервисы
%%
handle_info(disable_expires_timer, State) ->
    %% Do the action
    %io:format("DEBUG>> Service expire timer boooom ! ~n"),

    %% Получить из БД список всех сервисов, у которых истёк срок действия
    ExpireServices = db_api:get_expire_services(),
    %io:format("DEBUG>> Expire services: ~p~n", [ExpireServices]),

    case ExpireServices of
	{ok, Services} when is_list(Services) ->
	    %% пройтись по списку и вызвать соответствующий хэндлер
	    lists:foreach(fun(Service) -> 
				  case Service of
				      Service when is_record(Service, user_service) ->
					  case Service#user_service.type of
					      0 -> handler_vps:process_service(Service);
					      1 -> ok;
					      N when is_number(N) -> {error, unknown_service_type};
					      %% срабатывает, если у пользователя подключен неизвестный сервис
					      undefined-> {error, user_service_type_error}
					  end
				  end
			  end, Services);
	{error, Reason} -> {error, Reason}
    end,
    %% Start new timer
    erlang:send_after(?CHECK_INTERVAL, self(), disable_expires_timer),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




activate_user_service(UserService) when is_record(UserService, user_service) ->
    %% Пометить статус заказа в БД как 'поступивший'=1
    db_api:update_user_service_status(UserService#user_service.user_service_id, 1),
    spawn(fun() -> activate_user_service_(UserService) end);
activate_user_service(_UserService) -> invalid_record.


%% Web хостинг фиксированный
activate_user_service_(UserService) when UserService#user_service.type == 1 ->
    %% найти подходящий обслуживающий узел
    NodeRec = nodes_controller:get_suitable_node(UserService#user_service.type),
    case NodeRec of
	NodeRec when is_record(NodeRec, node) ->

	    io:format("DEBUG>> (PID: ~p) NodeRec: ~p! ~n UserService: ~p~n",[self(), NodeRec, UserService]),
	    %% сгенерировать имя пользователя и получить пароль из параметров сервиса
	    UserName = "test111",
	    Password = "qweqwewe",

	    %% добавить пользователя
	    Node = list_to_atom(lists:flatten(io_lib:format("cbnode@~ts", [binary:bin_to_list(NodeRec#node.ip)]))),
	    Args = [UserName, Password],
	    case rpc:call(Node, 'hosting_handler', 'add_user', Args) of
		ok ->
		    %% установить квоты
		    
		    io:format("DEBUG>> add_user sucesful !~n", []);
		error ->  ok   
	    end;
	not_found ->
	    %% Пометить сервис как обработанный, с кодом ошибки, что (узел не найден)=7
	    db_api:update_user_service_status(UserService#user_service.user_service_id, 7)
    end;

%% Web хостинг плавающий
activate_user_service_(UserService) when UserService#user_service.type == 2 ->
    %% найти подходящий обслуживающий узел
    Node = nodes_controller:get_suitable_node(UserService#user_service.type),
    case Node of
	Node when is_record(Node, node) ->
	    ok;
	not_found ->
	    %% Пометить сервис как обработанный, с кодом ошибки, что (узел не найден)=7
	    db_api:update_user_service_status(UserService#user_service.user_service_id, 7)
    end.


%% Проверить статус(=0) и режим обработки(=0)
%%case {UserService#order.processing_mode, Order#order.status} of
%%	{0,0} -> 
%%	_ -> error %% ошибка в модуле db_api
%%end;    
