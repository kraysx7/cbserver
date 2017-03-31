-module(country).

-export([get/1]).

-include("../include/iso3166.hrl").

get(#{country_alpha_2 := CountryAlpha2}) ->
    %% Получить структуру iso3166
    case iso3166_db:get(#{country_alpha_2 => CountryAlpha2}) of
	{ok, Iso3166} ->
	    Res = #{country_number => Iso3166#iso3166.country_number,
		    country_name => Iso3166#iso3166.country_name,
		    country_locale => Iso3166#iso3166.country_locale,
		    country_alpha_2 => Iso3166#iso3166.country_alpha_2,
		    country_alpha_3 => Iso3166#iso3166.country_alpha_3,
		    currency_alpha_3 => Iso3166#iso3166.currency_alpha_3, %% по iso4217
		    currency_number_3 => Iso3166#iso3166.currency_number_3}, %% по iso4217
	    {ok, Res};
	{error, not_found} -> {error, not_found}
    end.
