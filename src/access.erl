-module(access).

-export([add_user/1]).
-export([remove_user/1]).
-export([get_users/0]).
-export([get_allowed_actions/1]).
-export([check_access/1]).
-export([allow_action/1]).
-export([disallow_action/1]).

%% --------------------------------------------------------------------
%% добавить пользователя по id из таблицы user и установить role 10
%%
%% параметры:
%% #{user_id => UserId}
%% #{name => Name}
%% --------------------------------------------------------------------
add_user(Map) ->
    access_db:add_user(Map).

%% --------------------------------------------------------------------
%% удалить пользователя по id из таблицы user и установить role 0
%%
%% параметры:
%% #{user_id => UserId}
%% #{name => Name} 
%% --------------------------------------------------------------------
remove_user(Map) ->
    access_db:remove_user(Map).

%% --------------------------------------------------------------------
%% список пользователей-админов
%% --------------------------------------------------------------------
get_users() ->
    access_db:get_users().

%% --------------------------------------------------------------------
%% разрешенные действия для пользователя в списке access-рекордов
%% 
%% параметры:
%% #{user_id => UserId}
%% #{name => Name}
%% --------------------------------------------------------------------
get_allowed_actions(Map) ->
    access_db:get_allowed_actions(Map).

%% --------------------------------------------------------------------
%% проверка доступа к действию
%%
%% параметры:
%% #{user_id => UserId, controller := Controller, action := Action, closure := Closure}
%% #{name => Name, controller := Controller, action := Action, closure := Closure}
%% --------------------------------------------------------------------
check_access(Map) -> %% boolean()
    access_db:check_access(Map).

%% --------------------------------------------------------------------
%% разрешить пользователю определенное действие (если нет, добавляет в таблицу access)
%%
%% параметры:
%% #{user_id := UserId, controller := Controller, action := Action, closure := Closure}
%% #{name => Name, controller := Controller, action := Action, closure := Closure}
%% --------------------------------------------------------------------
allow_action(Map) ->
    access_db:allow_action(Map).

%% --------------------------------------------------------------------
%% запретить пользователю определенное действие (если есть, удаляет из таблицы access)
%%
%% параметры:
%% #{user_id := UserId, controller := Controller, action := Action, closure := Closure}
%% #{name => Name, controller := Controller, action := Action, closure := Closure}
%% --------------------------------------------------------------------
disallow_action(Map) ->
    access_db:disallow_action(Map).
