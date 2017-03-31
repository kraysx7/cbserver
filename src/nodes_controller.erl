-module(nodes_controller).
-behaviour(gen_server).

-include("../include/node.hrl").

%% API
-export([start_link/0, get_suitable_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CHECK_INTERVAL, 10000). % one second * 100
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Получить подходящий узел для обработки заданного сервиса
get_suitable_node(ServiceType) ->
    Key = {'_', ServiceType},
    ets:safe_fixtable(node, true),
    Values = ets:match_object(node, {Key, '$1'}),
    ets:safe_fixtable(node, false),
    case Values of
	[] -> not_found;
	Values when is_list(Values) ->
	    io:format("Values: ~p~n", [Values]),
	    random:seed(now()),
	    Index = random:uniform(length(Values)), %% выбирается случайный узел
	    {_, Value} = lists:nth(Index, Values),
	    Value
    end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
    io:format("DEBUG>> Nodes controller started!~n"),
    %% Создать таблицу node в ets
    ets:new(node, [set, named_table]),
    %% Запустить таймер тригера, обновляющего ets-кэш
    erlang:send_after(?CHECK_INTERVAL, self(), update_cache_trigger),
    {ok, #state{}}.


handle_info(update_cache_trigger, State) ->
    %% Получить из БД список узлов
    case db_api:get_enabled_nodes() of
	%% Обновить данные ets-кэше
	{ok, Nodes} ->
	    ets:safe_fixtable(node, true),
	    ets:delete_all_objects(node),
	    update_nodes_cache_(Nodes),
	    ets:safe_fixtable(node, false)

    end,

    %% Запустить новый таймер
    erlang:send_after(?CHECK_INTERVAL, self(), update_cache_trigger),
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

update_nodes_cache_([]) -> ok;
update_nodes_cache_([Node | Other]) ->
    NodeId = Node#node.node_id,
    case NodeId of
	NodeId when is_integer(NodeId) -> 
	    ServiceType = Node#node.service_type,
	    Key = {NodeId, ServiceType},
	    ets:insert(node, {Key, Node});
	_-> ok
    end,
    update_nodes_cache_(Other).
