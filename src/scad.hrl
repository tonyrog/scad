%% -*- erlang -*-
%% scad.hrl
-ifndef(__SCAD_HRL__).
-define( __SCAD_HRL__, true).

%% result structures from the eval stage
-type color_component() :: 0 | 1 | float().
-type color_name() :: binary().
-type color() :: [color_component()].
-type param() :: {Key::atom(), Value::term()}.
-define(DEFAULT_COLOR, (<<"Gold">>)).

-record(object,
	{
	 type = undefined :: none | cube | sphere | cylinder |
			     union | difference | intersection |
			     rotate | scale | translate | 
			     color,
	 params = [] :: [param()],
	 children = [] :: [#object{}]
	}).


%% strutures and types returned after the linting stage

-define(vararg, '*').
-define(is_var(X), is_list(X)).

-type var() :: string().
-type const() :: const_vector() | number() | binary() | boolean() | undef.
-type const_vector() :: {vector, [const()]}.

-type expr() ::
	const() |
	{range, {Start::expr(),Increment::expr(),End::expr()}} |
	{var, string()} |
	{op, Op::atom(), expr()} |
	{op, Op::atom(), expr(), expr()} |
	{op, Op::atom(), expr(), expr(), expr()} |
	{vector, [vector_element()]}.

-type vector_element() ::
	expr() |
	{lc_for, [var()], vector_element()} |
	{lc_forc, [var()], expr(), [var()], vector_element()} |
	{lc_let, [var()], vector_element()} |
	{lc_each, vector_element()} |
	{lc_if, expr(), vector_element()} |
	{lc_if, expr(), vector_element(), vector_element()}.

-record(mod,
	{
	 name :: string(),
	 use = [] :: [var()],
	 params = [] :: [var() | {var(),expr()}],
	 map = #{} :: #{var() => integer()},
	 stmt :: function() | any()
	}).

-record(func,
	{
	 name = anonymous :: anonymous | string(),
	 use = [] :: [var()],
	 params = [] :: [var() | {var(),expr()}],
	 map :: #{var() => integer()},
	 expr :: function() | any()
	}).



-endif. %% __SCAD_HRL__
