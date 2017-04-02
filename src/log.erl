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
<<<<<<< HEAD
%% Коммент фром  александр
=======
%% Ilya comment
>>>>>>> bf9854d9e4c702595a4d79e90a6fd82b0fd8740e


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
