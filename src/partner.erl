-module(partner).

-export([create/1, get/1, update/1, update_balance/2, get_users_stat/1]).

-include("../include/partner.hrl").

%% ---- create ----
%%
%% ParamsMap :
%%
create(ParamsMap) -> partner_db:create(ParamsMap).

%% ---- get ----
%%
%% ParamsMap :
%% #{remote_user_id := RemoteUserId}
%% #{remote_user_id := RemoteUserId, password := Password}
%% #{name := Name}
%%
%% return {ok, User#user} | {error, Reason}
get(ParamsMap) -> partner_db:get(ParamsMap).


%% ---- update ----
%%
%% ParamsMap :
%%
update(ParamsMap) -> partner_db:update(ParamsMap).

%% ---- update_balance ----
%%
%% PartnerId : int()
%% Cost : int()
update_balance(PartnerId, Cost) ->
    case partner_db:get(#{partner_id => PartnerId}) of
	{ok, Partner} ->
	    %% проверить возможность обновления и обновить баланс :
	    NewBalance = Partner#partner.balance + Cost,
	    if
		%% можно обновлять :
		NewBalance >= 0 ->
		    io:format("Partner ~p - UPDATE BALANCE! balance =~p~n", [PartnerId, NewBalance]),
		    case partner_db:update(#{field=>balance, value => NewBalance, partner_id => PartnerId}) of
			ok -> 
			    %% запустить процесс обновления баланса у партнёра-родителя
			    {ok, NewBalance}
		    end;
		true -> {error, low_balance, Partner#partner.balance}
	    end;
	{error, not_found} -> {error, user_not_found}
    end.


%%%===================================================================
%%% STATISTIC FUNCTIONS
%%%===================================================================

%% Функция возвращает статистику по пользователям партнёра :
%% StatKey :=
%% user_name : Имя пользователя
%% purchaces_count : Количество покупок
%% income : Потраченные средства

%% StatElement := [{StatKey, StatVal}, ... ]

%% return {ok, [StatElement, ... ]}
get_users_stat(PartnerId) ->
     partner_db:get_users_stat(PartnerId).
