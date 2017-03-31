-module(order).

-export([create/1, get/1, update/1, close/1, set_status/2]).

create(#{user_id := UserId, service_id := ServiceId, params := OrderParams} = ParamsMap) -> 
    %% Проверить UserId и ServiceId
    UserRes = user:get(#{user_id => UserId}),
    ServiceRes = service:get(#{service_id => ServiceId}),
    case {UserRes, ServiceRes} of
	{{ok, _User}, {ok, _Service}} ->
	    OrderParamsStr = lists:flatten(io_lib:format("~p.", [OrderParams])),	
	    UpdatedParamsMap = maps:merge(ParamsMap, #{params => OrderParamsStr}),

	    order_db:create(UpdatedParamsMap);
	{{error, not_found}, _} -> {error, user_not_found};
	{_, {error, not_found}} -> {error, service_not_found}
    end.


get({user_id, UserId}) -> order_db:get({user_id, UserId});
get(ParamsMap) -> order_db:get(ParamsMap).


update(#{order_id := _OrderId, params := OrderParams} = ParamsMap) ->
    OrderParamsStr = lists:flatten(io_lib:format("~p.", [OrderParams])),
    UpdatedParamsMap = maps:update(params, OrderParamsStr, ParamsMap),
    order_db:update(UpdatedParamsMap);

update(ParamsMap) -> order_db:update(ParamsMap).


close(#{order_id := OrderId, status := Status}) ->
    CloseDate = calendar:local_time(),
    order_db:update(#{order_id => OrderId, status => Status, close_date => CloseDate}).



set_status(OrderId, Status) -> order_db:set_status(OrderId, Status).









