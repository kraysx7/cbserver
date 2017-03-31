-module(sql_builder).

-export([build_expr/1, attach_expr/3]).


build_expr(Tokens) -> build_expr(Tokens, []).
build_expr([], Result) -> Result;
build_expr([Token | Tokens], Result) ->
    {Predicat, Expr} = Token,
    build_expr(Tokens, attach_expr(Predicat, Expr, Result)).


attach_expr(_Predicat, undefined, ExprList) -> ExprList;
attach_expr(_Predicat, Expr, [] = ExprList) ->
    lists:flatten(lists:concat([ExprList, Expr]));
attach_expr(Predicat, Expr, ExprList) ->
     lists:flatten(lists:concat([ExprList, Predicat, Expr])).






