-module(node_db).

-export([get/1]).

-include("../include/mysql.hrl").
-include("../include/node.hrl").


get(#{node_id := NodeId}) ->
    emysql:prepare(get_node_by_node_id_stmt, <<"SELECT * FROM `node` WHERE `node_id` = ?">>),
    Result = emysql:execute(main_pool, get_node_by_node_id_stmt, [NodeId]),
    RecResult = emysql:as_record(Result, node, record_info(fields, node)), 
    case RecResult of
	[Node | _] when is_record(Node, node) -> {ok, Node};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{service_type := ServiceType}) ->
    emysql:prepare(get_node_by_service_type_stmt, <<"SELECT * FROM `node` WHERE `service_type` = ?">>),
    Result = emysql:execute(main_pool, get_node_by_service_type_stmt, [ServiceType]),
    RecResult = emysql:as_record(Result, node, record_info(fields, node)), 
    case RecResult of
	[Node | _] when is_record(Node, node) -> {ok, RecResult};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;

get(#{status := Status}) ->
    emysql:prepare(get_node_by_status_stmt, <<"SELECT * FROM `node` WHERE `status` = ?">>),
    Result = emysql:execute(main_pool, get_node_by_status_stmt, [Status]),
    RecResult = emysql:as_record(Result, node, record_info(fields, node)), 
    case RecResult of
	[Node | _] when is_record(Node, node) -> {ok, RecResult};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.
