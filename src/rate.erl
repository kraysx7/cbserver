-module(rate).

-export([get/1]).

-include("../include/rate.hrl").

get(ParamsMap) ->
    rates_srv:get_rate(ParamsMap).
