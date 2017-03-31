-module(ip2location).

-export([get/1]).

-include("../include/iso3166.hrl").
-include("../include/ip2location_db1.hrl").

get(#{ipv4 := IpV4}) ->
    %% Получить структуру ip2location_db1
    case ip2location_db:get(#{ipv4 => IpV4, version => db1}) of
	{ok, Ip2Location} ->
	    CountryCode = Ip2Location#ip2location_db1.country_code,
	    CountryName = Ip2Location#ip2location_db1.country_name,
	    
	    %% Получить локаль страны из iso3166
	    %% CountryLocale = case iso3166_db:get(#{country_alpha_2 => CountryCode}) of
	    %% 			{ok, Iso3166} ->
	    %% 			    Iso3166#iso3166.country_locale; 
	    %% 			_ -> "en"
	    %% 		    end,
	    
	    Res = #{country_code => CountryCode, country_name => CountryName},
	    {ok, Res};
	{error, not_found} -> {error, not_found}
    end.
    
%% node_db:get(ParamsMap).
