-module(ip2location_db).

-include("../include/mysql.hrl").
-include("../include/ip2location_db1.hrl").

-export([get/1]).

%%%================= Функции работы c таблицами ip2location =================

%% Получение структуры ip2location_db1 на основе IP:V4 адреса
get(#{ipv4 := IpV4, version := db1}) ->
    emysql:prepare(get_ipv4_db1_stmt, <<"SELECT * FROM `ip2location_v4_db1` WHERE `ip_from` <= ? AND `ip_to` >= ?">>),
    Result = emysql:execute(main_pool, get_ipv4_db1_stmt, [IpV4, IpV4]),
    RecResult = emysql:as_record(Result, ip2location_db1, record_info(fields, ip2location_db1)), 
    case RecResult of
	[Loc | _] when is_record(Loc, ip2location_db1) -> {ok, Loc};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


