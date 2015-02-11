-module(user).

-export([register/3, login/2, logout/2]).

%% IN DEV
register(UserName, Password, Email) ->
    io:format("DEBUG>> user:register (pid: ~p)~n", [self()]),
    %% Добавить пользователя в базу
    UserId = db_api:create_user(UserName, Password, Email),    
    {ok, UserId}.

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
