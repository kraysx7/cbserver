-module(access_db).

-include("../include/user.hrl").
-include("../include/mysql.hrl").
-include("../include/access.hrl").

-compile({no_auto_import,[get/1]}).

-export([add_user/1]).
-export([remove_user/1]).
-export([get_users/0]).
-export([get_allowed_actions/1]).
-export([check_access/1]).
-export([get/1]).
-export([allow_action/1]).
-export([disallow_action/1]).

%% роли:
-define(admin, 10).
-define(superuser, 20).

%% --------------------------------------------------------------------
%% наделяем пользователя админскими правами, установливая user.role = ?admin
%%
%% параметры:
%% #{user_id => UserId}
%% --------------------------------------------------------------------
add_user(#{user_id := UserId}) -> %%%%
    {ok, UserData} = user:get(#{user_id => UserId}),
    case UserData#user.role of
        ?superuser ->
	    sorry;
	_ ->
	    user_db:update(#{user_id => UserId, role => ?admin})
    end;
add_user(#{name := Name}) ->
    {ok, UserData} = user:get(#{name => Name}),
    add_user(#{user_id => UserData#user.user_id}).

%% --------------------------------------------------------------------
%% удалить пользователя по id из таблицы user и установить role 0
%%
%% параметры:
%% #{user_id => UserId}
%% --------------------------------------------------------------------
remove_user(#{user_id := UserId}) -> %%%%
    {ok, UserData} = user:get(#{user_id => UserId}),
    case UserData#user.role of
	?superuser ->
	    sorry;
	_ ->
	    user_db:update(#{user_id => UserId, role => 0})
    end;
remove_user(#{name := Name}) ->
    {ok, UserData} = user:get(#{name => Name}),
    remove_user(#{user_id => UserData#user.user_id}).

%% --------------------------------------------------------------------
%% список пользователей с именами (join из user)
%% --------------------------------------------------------------------
get_users() -> %%%%
    user_db:get(#{role => ?admin}).

%% --------------------------------------------------------------------
%% разрешенные действия для пользователя как список access-рекордов
%%
%% параметры:
%% #{user_id => UserId}
%% --------------------------------------------------------------------
get_allowed_actions(#{user_id := UserId}) -> %%%%
    emysql:prepare(get_allowed_actions_stmt, <<"SELECT * FROM `access` WHERE `user_id`=?">>),
    Result = emysql:execute(main_pool, get_allowed_actions_stmt, [UserId]),
    RecResult = emysql:as_record(Result, access, record_info(fields, access)),
    case RecResult of
	[User | _] when is_record(User, access) -> {ok, RecResult};
	[] -> {error, not_found};
	_  -> {error, undefined}
    end;
get_allowed_actions(#{name := Name}) ->
    {ok, UserData} = user:get(#{name => Name}),
    get_allowed_actions(#{user_id => UserData#user.user_id}).

%% --------------------------------------------------------------------
%% проверить наличие действия в access
%%
%% параметры:
%% #{user_id => UserId, controller := Controller, action := Action, closure := Closure}
%% --------------------------------------------------------------------
get(#{user_id := UserId, controller := Controller, action := Action, closure := Closure}) -> %%%
    emysql:prepare(get_stmt, <<"SELECT * FROM `access` WHERE `user_id`=? AND `controller`=? AND `action`=? AND `closure`=?">>),
    Result = emysql:execute(main_pool, get_stmt, [UserId, Controller, Action, Closure]),
    RecResult = emysql:as_record(Result, access, record_info(fields, access)),
        case RecResult of
        [User | _] when is_record(User, access) ->
		{ok, RecResult};
	    [] -> {error, not_found};
	    _  -> {error, undefined}
	end;
get(#{name := Name, controller := Controller, action := Action, closure := Closure}) ->
    {ok, UserData} = user:get(#{name => Name}),
    get(#{user_id => UserData#user.user_id, controller => Controller, action => Action, closure => Closure}).

%% --------------------------------------------------------------------
%% проверить наличие действия в access (булева)
%%
%% параметры:
%% #{user_id => UserId, controller := Controller, action := Action, closure := Closure}
%% --------------------------------------------------------------------
check_access(#{user_id := UserId, controller := Controller, action := Action, closure := Closure} = Map) ->
    {ok, UserData} = user:get(#{user_id => UserId}),
    case UserData#user.role of
	?superuser ->
	    true;
	?admin ->
	    case get(Map) of
		{ok, _} -> true;
		_ -> false
	    end;
	_ ->
	    false
    end;
check_access(#{name := Name, controller := Controller, action := Action, closure := Closure}) ->
    {ok, UserData} = user:get(#{name => Name}),
    case UserData#user.role of
	?superuser ->
	    true;
	?admin ->
            case get(#{user_id => UserData#user.user_id, controller => Controller, action => Action, closure => Closure}) of
		{ok, _} -> true;
		_ -> false
	    end;
        _ ->
            false
    end.    

%% --------------------------------------------------------------------
%% разрешить пользователю определенное действие (если нет, добавляет в access)
%%
%% параметры:
%% #{user_id => UserId, controller := Controller, action := Action, closure := Closure}
%% --------------------------------------------------------------------
allow_action(#{user_id := UserId, controller := Controller, action := Action, closure := Closure} = Map) -> %%%
    case access_db:get(Map) of
	{error, not_found} ->
	    create(Map);
	{error, undefined} ->
	    {error, undefined};
	_ ->
	    {error, already_exist}
    end;
allow_action(#{name := Name, controller := Controller, action := Action, closure := Closure}) ->
    {ok, UserData} = user:get(#{name => Name}),
    allow_action(#{user_id => UserData#user.user_id, controller => Controller, action => Action, closure => Closure}).

create(#{user_id := UserId, controller := Controller, action := Action, closure := Closure}) ->
    emysql:prepare(allow_action_stmt, <<"INSERT `access` SET `user_id`=?, `controller`=?, `action`=?, `closure`=?">>),
    Result = emysql:execute(main_pool, allow_action_stmt, [UserId, Controller, Action, Closure]),
                case Result of
                OkPacket when is_record(OkPacket, ok_packet) ->
			ok;
		    _ -> {error, db_error}
		end;
create(#{name := Name, controller := Controller, action := Action, closure := Closure}) ->
    {ok, UserData} = user:get(#{name => Name}),
    create(#{user_id => UserData#user.user_id, controller => Controller, action => Action, closure => Closure}).

%% --------------------------------------------------------------------
%% запретить пользователю определенное действие (если есть, удаляет из allowed_actions)
%%
%% параметры:
%% #{user_id => UserId, action => {имя контроллера, имя действия}}
%% --------------------------------------------------------------------
disallow_action(#{user_id := UserId, controller := Controller, action := Action, closure := Closure}) -> %%%
	    emysql:prepare(disallow_action_stmt, <<"DELETE FROM `access` WHERE `user_id`=? AND `controller`=? AND `action`=? AND `closure`=?">>),
	    Result = emysql:execute(main_pool, disallow_action_stmt, [UserId, Controller, Action, Closure]),
	    case Result of
		OkPacket when is_record(OkPacket, ok_packet) -> ok;
		_ -> {error, db_error}
	    end;
disallow_action(#{name := Name, controller := Controller, action := Action, closure := Closure}) ->
    {ok, UserData} = user:get(#{name => Name}),
    disallow_action(#{user_id => UserData#user.user_id, controller => Controller, action => Action, closure => Closure}).

