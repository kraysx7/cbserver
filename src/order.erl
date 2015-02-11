-module(order).

-export([create/5]).

create(UserId, ServiceId, Params, Period, ProcessingMode) ->
    Status = 0, %% требует обработки
    ProcessingMode = 0, %% обработать автоматически
    CreateDate = calendar:local_time(),
    
    ParamsStr = lists:flatten(io_lib:format("~p.", [Params])),
    
    io:format("DEBUG>> order:create (pid: ~p)~n", [self()]),
    
    %% Добавить заказ в базу
    OrderId = db_api:create_order(UserId, ServiceId, ParamsStr, Period, Status, ProcessingMode, CreateDate, null),    
    {ok, OrderId}.
