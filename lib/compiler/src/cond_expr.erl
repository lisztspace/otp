-module(cond_expr).

-export([parse_transform/2, parse_transform_info/0]).

-spec parse_transform(Forms, Options) -> NewForms when
      Forms :: [erl_parse:abstract_form() | erl_parse:form_info()],
      NewForms :: Forms,
      Options :: [compile:option()].
parse_transform(Forms, _Options) ->
    forms(Forms).

-spec parse_transform_info() -> #{ 'error_location' => 'column' | 'line' }.
parse_transform_info() ->
    #{error_location => column}.


forms(Forms) ->
  [form(Form) || Form <- Forms].

form({function,Anno,Name0,Arity0,Clauses0}) ->
  {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
  {function,Anno,Name,Arity,Clauses};
form(Form) ->
  Form.

function(Name, Arity, Clauses0) ->
    {Name, Arity, clauses(Clauses0)}.

clauses(Clauses) ->
  [clause(Clause) || Clause <- Clauses].

clause({clause, Anno, Head, Guard, Body}) ->
  {clause, Anno, Head, Guard, exprs(Body)}.

exprs(Exprs) ->
  [expr(Expr) || Expr <- Exprs].

expr({'cond', Anno, Clauses}) ->
  transform_cond_clauses(Clauses, Anno);
expr(Expr) ->
  Expr.

transform_cond_clauses([], Anno) ->
  {call,Anno,{atom,Anno,error}, [{atom,Anno,cond_clause}]};
transform_cond_clauses([Clause | Clauses], Anno) ->
  %% transform rest of clauses first so they can be inserted into the
  %% transform/expansion of the first clause
  Rest = transform_cond_clauses(Clauses, Anno),
  transform_cond_clause(Clause, [Rest]).

transform_cond_clause({cond_clause, Anno, [], [Expr], Guard, Body}, Rest) ->
  {'case', Anno, Expr, [{clause, Anno, [{atom, Anno, true}], Guard, Body},
                        {clause, Anno, [{var, Anno, '_'}], [],
                         Rest}]};
transform_cond_clause({cond_clause, Anno, Pat, [Expr], Guard, Body}, Rest) ->
  {'case', Anno, Expr, [{clause, Anno, Pat, Guard, Body},
                        {clause, Anno, [{var, Anno, '_'}], [],
                         Rest}]}.

