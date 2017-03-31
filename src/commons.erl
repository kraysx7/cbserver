-module(commons).
-compile([export_all]).

-include("../include/user.hrl").
-include("../include/user_service.hrl").
-include("../include/service.hrl").
-include("../include/order.hrl").

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

%% ----------------------------------------------------------

list_to_integer(List) when is_list(List) ->
    case re:run(List, "^[0-9]*$") of
	{match, _} -> erlang:list_to_integer(List);
	nomatch -> badarg	  
    end;

list_to_integer(_) -> badarg.

list_to_float(List) when is_list(List)->
    case io_lib:fread("~f", List) of
	{ok, [Float], []} -> Float;
	_ ->
	    case io_lib:fread("~d", List) of
		{ok, [Int], []} -> float(Int);
		_ -> badarg
	    end
    end;

list_to_float(_) -> badarg.

%% ----------------------------------------------------------

utc_to_datetime(UtcSeconds) ->
   BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
   Seconds       = BaseDate + UtcSeconds,
   {Date,Time} = calendar:gregorian_seconds_to_datetime(Seconds),
   {Date,Time}.

-define(UNIX_EPOCH_MAGIC, 62167219200).
datetime_to_utc(DateTime) ->
    UtcSeconds = case DateTime of
		     undefined -> 0;
		     {{_,_,_},{_,_,_}} = DT ->
			 calendar:datetime_to_gregorian_seconds(DT)-?UNIX_EPOCH_MAGIC;
		     {datetime, {{_,_,_},{_,_,_}} = DT2} ->
			 calendar:datetime_to_gregorian_seconds(DT2)-?UNIX_EPOCH_MAGIC
		 end,
    UtcSeconds.

%% ----------------------------------------------------------

decode_service_params(ParamsBin) when byte_size(ParamsBin) > 0 ->
    {ok, T, _} = erl_scan:string(binary_to_list(ParamsBin)),
    try erl_parse:parse_term(T) of
	{ok, TermParams} -> TermParams;
	{error, Reason} ->
	    io:format("~nDEBUG>>> commons:decode_service_params parse Error: ~p~nParams: ~p~n", [Reason, binary_to_list(ParamsBin)]),
	    []
    catch
	Error ->
	    io:format("~nDEBUG>>> commons:decode_service_params catch Error: ~p~n", [Error]),
	    []
    end;

decode_service_params(ParamsBin) -> [].

records_to_tuplelist(Services, ServicesRecName) when is_list(Services) ->
    lists:foldl(
      fun(Service, R) ->
	      ServiceData = record_to_tuplelist(Service, ServicesRecName),
	      lists:append(R, [ServiceData])
      end, [], Services).
 
record_to_tuplelist(Service, service) ->
    [
     {service_id, Service#service.service_id},
     {name, Service#service.name},
     {type, Service#service.type},
     {params, Service#service.params},
     {cost, Service#service.cost},
     {period, Service#service.period},
     {status, Service#service.status}
    ];

record_to_tuplelist(Service, user_service) ->
    [
     {user_id, Service#user_service.user_id},
     {service_id, Service#user_service.service_id},
     {type, Service#user_service.type},
     {name, Service#user_service.name},
     {params, Service#user_service.params},
     {end_date, Service#user_service.end_date},
     {status, Service#user_service.status}
    ].

is_logined(SessionID) ->
    case boss_session:get_session_data(SessionID, "user_data") of
	UserData when is_record(UserData, user) -> {ok, UserData};
	undefined -> {error, not_logined};
	{error, Reason} -> {sys_error, Reason};
	_ -> {sys_error, session_data_corrupt}
    end.
