-module(user).

-export([create/1, get/1, update/1, update_balance/2, register/3, login/2, logout/2]).

-include("../include/user.hrl").

%% ---- create ----
%%
%% ParamsMap :
%%
create(ParamsMap) -> user_db:create(ParamsMap).



%% ---- get ----
%%
%% ParamsMap :
%% #{remote_user_id := RemoteUserId}
%% #{remote_user_id := RemoteUserId, password := Password}
%% #{name := Name}
%% #{email := EMail}
%% #{user_id := UserId}
%% #{name := UserName, password := Password}

%% return {ok, User#user} | {error, Reason}
get(ParamsMap) -> user_db:get(ParamsMap).


%% ---- update ----
%%
%% ParamsMap :

update(#{user_id := UserId, params := Params}) ->
    ParamsStr = lists:flatten(io_lib:format("~p.", [Params])),
    user_db:update(#{user_id => UserId, params => ParamsStr});

update(ParamsMap) -> user_db:update(ParamsMap).



%% ---- update_balance ----
%%
%% UserId : int()
%% Cost : int()
update_balance(UserId, Cost) ->
    case user_db:get(#{user_id => UserId}) of
	{ok, User} ->
	    %% проверить возможность обновления и обновить баланс :
	    NewBalance = User#user.balance + Cost,
	    if
		%% можно обновлять :
		NewBalance >= 0 ->
		    io:format("User NewBalance ~p~n", [NewBalance]),
		    case user_db:update(#{user_id => UserId , balance => NewBalance}) of
			ok -> {ok, NewBalance}
		    end;
		true -> {error, low_balance, User#user.balance}
	    end;
	{error, not_found} -> {error, user_not_found}
    end.


%% return: {ok, UserId}.
register(UserName, Password, EMail) ->
    io:format("DEBUG>> user:register (~p ~p ~p)~n", [UserName, Password, EMail]),
    user_db:create_user(UserName, Password, EMail).

%% IN DEV
login(UserName, Password) ->
    UserData = [],
    SessionKey = "f4gsds5353sdvnuisdys5459d7dgfhds8444hg47",
    {ok, UserData, SessionKey}.

%% IN DEV
logout(UserId, SessionKey) ->
    %% Получить объект сессии из кэша на основании ключа
    case user_session:get(SessionKey) of
	{error, not_found} -> {error, session_not_found};
	{error, session_expire} -> {error, user_not_logined}
	%% Сессия найдена, проверить идентификатор пользователя и удалить сессию
	%%{ok, UserSession} when UserSession#user_session.user_id = UserId ->
	%%    user_session:remove(SessionKey), ok
    end.






%% create(#{remote_user_id := RemoteUserId,
%% 	 user_name := UserName,
%% 	 password := Password, 
%% 	 email := EMail}) -> 
%%     ParamsMap = maps:merge(#{remote_user_id => RemoteUserId, remote_auth_type => 0}, 
%% 			   #{user_name => UserName, password => Password, email => EMail}),
%%     io:format("1~n"),
%%     user_db:create(ParamsMap);


%% create(#{remote_user_id := RemoteUserId, remote_auth_type := RemoteAuthType, password := Password}) -> 
%%     ParamsMap = maps:merge(#{remote_user_id => RemoteUserId, remote_auth_type => RemoteAuthType},
%% 			   #{user_name => "", password => Password, email => ""}),
%%     io:format("2~n"),
%%     user_db:create(ParamsMap);


%% create(#{remote_user_id := RemoteUserId, password := Password}) -> 
%%     ParamsMap = maps:merge(#{remote_user_id => RemoteUserId},
%% 			   #{user_name => "", password => Password, email => ""}),
%%     io:format("3~n"),
%%     user_db:create(ParamsMap);
