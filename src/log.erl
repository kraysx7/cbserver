-module(log).

-export([create/1, get/1, get_templates/0]).

-include("../include/log.hrl").


%% ---- add ----
%%
%% ParamsMap (Параметры для создания записи в формате Maps):
%% #{log_id => LogId,
%%   action_type => ActionType,
%%   action_name => ActionName,
%%   user_id => UserId,
%%   user_name => UserName,
%%   creation_time => DateTime,
%%   params => Params,
%%   ip => Ip
%%  }
%%
create(ParamsMap) -> log_db:create(ParamsMap).

any_super_fun2() -> ok.

%% ---- get ----
%%
%% ParamsMap (Параметры для выборки в формате Maps):
%% #{log_id => LogId,
%%   action_type => ActionType,
%%   action_name => ActionName,
%%   user_id => UserId, 
%%   user_name => UserName,
%%   creation_time => DateTime,
%%   params => Params,
%%   ip => Ip
%%  }
%%
get(ParamsMap) -> log_db:get(ParamsMap).

get_templates() -> log_db:get_templates().
