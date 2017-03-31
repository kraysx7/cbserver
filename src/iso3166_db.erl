-module(iso3166_db).

-include("../include/mysql.hrl").
-include("../include/iso3166.hrl").

-export([get/1]).

%%%================= Функции работы c таблицой iso3166 =================

%% Получение iso3166 на основе кода страны
get(#{country_alpha_2 := CountryAlpha2}) ->
    emysql:prepare(get_iso3166_by_ca2_stmt, <<"SELECT * FROM `iso3166` WHERE `country_alpha_2` = ?">>),
    Result = emysql:execute(main_pool, get_iso3166_by_ca2_stmt, [CountryAlpha2]),
    RecResult = emysql:as_record(Result, iso3166, record_info(fields, iso3166)), 
    case RecResult of
	[Iso3166 | _] when is_record(Iso3166, iso3166) -> {ok, Iso3166};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================


