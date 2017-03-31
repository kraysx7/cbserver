-module(promocode_db).

-export([get/1, update/1]).

-include("include/mysql.hrl").
-include("include/promocode.hrl").


get(#{key := Key}) ->
    emysql:prepare(get_promocode_by_key_stmt, <<"SELECT * FROM `promocode` WHERE `key` = ?">>),
    Result = emysql:execute(main_pool, get_promocode_by_key_stmt, [Key]),
    RecResult = emysql:as_record(Result, promocode, record_info(fields, promocode)), 
    case RecResult of
	[Promocode | _] when is_record(Promocode, promocode) -> {ok, decode_params(Promocode)};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end.


update(#{promocode_id := PromocodeId, status := Status}) ->
    emysql:prepare(update_promocode_status_stmt, <<"UPDATE `promocode` SET `status` = ? WHERE `promocode_id` = ?">>),
    Result = emysql:execute(main_pool, update_promocode_status_stmt, [Status, PromocodeId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end;

update(#{promocode_id := PromocodeId, activate_cur := ActivateCur}) ->
    emysql:prepare(update_promocode_actcur_stmt, <<"UPDATE `promocode` SET `activate_cur` = ? WHERE `promocode_id` = ?">>),
    Result = emysql:execute(main_pool, update_promocode_actcur_stmt, [ActivateCur, PromocodeId]),
    case Result of
	OkPacket when is_record(OkPacket, ok_packet) -> ok;
	_ -> {error, db_error}
    end.





%%%===================================================================
%%% Internal functions
%%%===================================================================


decode_params(Records) when is_list(Records) ->
    lists:foldl(
      fun(Record, R) ->
	      UpdatedRecord = decode_params(Record),
	      lists:append(R, [UpdatedRecord])
      end, [], Records);

decode_params(Record) when is_record(Record, promocode) ->
    TermParams = commons:decode_service_params(Record#promocode.params),
    %% модифицируем сруктуру сервиса
    Record#promocode{params=TermParams}.
