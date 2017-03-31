-module(node).

-export([get/1]).

-include("../include/node.hrl").

get(ParamsMap) ->
    node_db:get(ParamsMap).
