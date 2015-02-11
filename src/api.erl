%% В данном модуле перечисленны все функции для доступа к биллинг системе (API)
%% 
-module(api).
-author("Ilya Troshkov").
-export([login/2]).

%% Функция получает список всех сервисов доступных для подключения
get_services() ->
    ok.

%% Регистрация нового пользователя
register(UserName, Password, Email) ->
    ok.

%% Авторизация
%% 
%% 
login(UserName, Password) ->
    ok.


logout(UserName, SessionKey) ->
    ok.

update_user_info(UserId, NewUserData, SessionKey)->
    ok.

create_order(UserId, ServiceId, SessionKey) ->
    ok.

get_orders(UserId, SessionKey) ->
    ok.

get_user_services(UserId, SessionKey) ->
    ok.

get_user_service_info(UserId, UserServiceId, SessionKey) ->
    ok.

disable_user_service(UserId, UserServiceId, SessionKey) ->
    ok.

delete_user_service(UserId, UserServiceId, SessionKey) ->
    ok.


