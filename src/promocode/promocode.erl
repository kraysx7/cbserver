-module(promocode).

-export([get/1, update/1]).

-include("include/user_promocode.hrl").

%% ---- get ----
%%
%% ParamsMap : #{...}
%%
get(ParamsMap) -> promocode_db:get(ParamsMap).


%% ---- update ----
%%
%% ParamsMap : #{...}
%%
update(ParamsMap) -> promocode_db:update(ParamsMap).


