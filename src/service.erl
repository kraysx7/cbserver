-module(service).

-export([get/1]).

%% FUNCTION: service:get
%%
%% ParamsMap :
%% #{remote_user_id := RemoteUserId}
%% #{remote_user_id := RemoteUserId, password := Password}
%% #{name := Name}
%% #{email := EMail}
%% #{user_id := UserId}
%% #{name := UserName, password := Password}

%% return {ok, User#user} | {error, Reason}
get(ParamsMap) -> service_db:get(ParamsMap).

