%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    wings plugin to import OpenSCAD files.
%%% @end
%%% Created : 18 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(wpc_scad).

-export([import/1]).
-export([string/1]).

-compile([export_all]).



-type color_component() :: 0 | 1 | float().
-type color_name() :: binary().
-type color() :: [color_component()].
-type param() :: {Key::atom(), Value::term()}.
-define(DEFAULT_COLOR, (<<"Gold">>)).

-record(transform,
	{
	 type = id :: id | rotate | scale | translate,
	 params = [] :: [param()]
	}).

-record(color,
	{
	 c = ?DEFAULT_COLOR :: color_name() | color(),
	 alpha = undef ::  undef | color_component()
	}).

-record(object,
	{
	 type = undefined :: none | cube | sphere | cylinder |
			     union | difference | intersection,
	 params = [] :: [param()],
	 children = [] :: [#object{}],
	 transforms = [] :: [#transform{}],
	 color = #color{} :: #color{}
	}).
	
import(Filename) ->
    {ok,Ast} = scad:parse_file(Filename),
    scad_import(Ast, Filename).

string(Data) ->
    {ok,Ast} = scad:parse_string(Data),
    scad_import(Ast, "*string*").

scad_import(Ast) ->
    scad_import(Ast, "*ast*").

%% be inspired by wings/plugins_src/import_export/x3d_import.erl

%% split_assinments must work a bit different!!!?
%% we must keep the position of the assignment in the list,
%% but of the last assignment
%% FIXME: "precompile" the AST to make it easier to handle
%%	  assignments and scopes
%%
%%  {module, Line, {id, Line, Name}, Params, Statement}
%%     is analyzed recursively and replaced with:
%%  {mod, Line, Name, Params, Bind, Statement'}
%%     where Bind are the global variables used by the module
%%     and Statement' is the statement list with updated assignments
%%     and anonomous blocks removed
%%
%%  {function, Line, {id, Line, Name}, Params, Expr}
%%     is analyzed recursively and replaced with:
%%  {func, Line, Name, Params, Bind, Expr}
%%     where Bind are the global variables used by the module
%%     and Statement' is the statement list with updated assignments
%% 
scad_import(Stmts, Filename) when is_list(Stmts) ->
    %% setup default variable values "$fn" etc
    Scope = default_scope(Filename),
    {Stmts1,_As} = move_assignments(Stmts),
    %%io:format("assignments:\n~p\n", [sets:to_list(As)]),
    %%io:format("statements:\n~p\n", [Stmts1]),
    eval_stmt_list(Stmts1, Scope).

eval_stmt_list(Stmts, Scope) ->
    eval_stmt_list_(Stmts, Scope, []).

eval_stmt_list_([Stmt|Stmts], Scope, Acc) ->
    {A,Scope1} = eval_stmt(Stmt, Scope),
    eval_stmt_list_(Stmts, Scope1, [A|Acc]);
eval_stmt_list_([], Scope, Acc) ->
    {lists:reverse(Acc), Scope}.

eval_stmt(Stmt, Scope) ->
    case Stmt of
	empty ->
	    {[], Scope};  %% empty?
	{block,_Line,Stmts} -> %% not a scope
	    eval_stmt_list(Stmts, Scope);
	{module,_Line,{id,_,Name},Params,Statement} ->
	    %% FIXME: here we should include assignments from
	    %% the parent scope and move assignments to the
	    %% beginning of the statement list when that is the case
	    {Statement1,As} = move_assignments(Statement),
	    {Use1,Ps} = use_params(Params, sets:new()),
	    Use2 = use_stmt(Statement1, Use1),
	    Locals = sets:union(Ps, As),
	    Parent = sets:subtract(Use2, Locals),
	    Bound = get_vars(Parent, Scope),
	    %%io:format("module ~p\n  assign=~p\n  use=~p\n  bound=~p\n~p\n\n", 
	    %% [Name,sets:to_list(As),sets:to_list(Use2),
	    %% Bound, Statement1]),
	    Ms = maps:get(modules,Scope,[]),
	    Ms1 = maps:put(Name, {Bound,Params,Statement1}, Ms),
	    {true,Scope#{ modules => Ms1 }};
	{function,_Line,{id,_,Name},Params,Expr} ->
	    Ps = eval_params(Params, Scope),
	    Fs = maps:get(functions,Scope,[]),
	    Fs1 = maps:put(Name, {Ps,Expr}, Fs),
	    {true,Scope#{ functions => Fs1 }};
	{assign,_Line,{id,_,Var},Expr} ->
	    Value = eval_expr(Expr, Scope),
	    {true, set(Var, Value, Scope)};
	{mcall,_Line,_Tags,{id,_,"let"},Args,ChildStmt} ->
	    Scope1 = eval_let_args(Args, Scope),
	    eval_stmt(ChildStmt, Scope1);
	%% assign is deprecated but used
	{mcall,_Line,_Tags,{id,_,"assign"},Args,ChildStmt} ->
	    Scope1 = eval_let_args(Args, Scope),
	    eval_stmt(ChildStmt, Scope1);
	{mcall,_Line,_Tags,{id,_,"for"},Args,ChildStmt} ->
	    eval_for(Args, ChildStmt, Scope);
	{mcall,_Line,Tags,{id,_,ID},Args, ChildStmt} ->
	    Obj = eval_mcall(ID, Args, Tags, ChildStmt, Scope),
	    %% should return object/transformation/operator
	    {Obj, Scope};
	{'if', _Line, Cond, Then, Else} ->
	    C = eval_expr(Cond, Scope),
	    case bool(C) of
		true -> eval_stmt(Then, Scope);
		false -> eval_stmt(Else, Scope)
	    end;
	{'if', _Line, Cond, Then} ->
	    C = eval_expr(Cond, Scope),
	    case bool(C) of
		true -> eval_stmt(Then, Scope);
		false -> {true,Scope}
	    end
    end.

%% fo (v1 = range1, ...  vn = rangen) { stmts }
eval_for([{'=',_Ln,{id,_,Var},Range}|Args], Stmts, Scope) ->
    case eval_expr(Range, Scope) of
	{range,{S,I,E}} ->
	    fold_sequence(S, E, I,
			  fun(Val, {Acc,Sa}) ->
				  Sb = set(Var, Val, Sa),
				  {Obj,Sc} = eval_for(Args, Stmts, Sb),
				  {[Obj|Acc],Sc}
			  end, {[],Scope});
	Vec when is_list(Vec) ->
	    lists:foldl(fun(Val, {Sa,Acc}) ->
				Sb = set(Var, Val, Sa),
				{Obj,Sc} = eval_for(Args, Stmts, Sb),
				{[Obj|Acc],Sc}
			end, {[],Scope}, Vec)
    end;
eval_for([], Smts, Scope) ->
    eval_stmt(Smts, Scope).


eval_assignment_list([{assign,_Ln0,{id,_Ln,Var},Expr}|As], Scope) ->
    Value = eval_expr(Expr, Scope),
    Scope1 = set(Var, Value, Scope),
    eval_assignment_list(As, Scope1);
eval_assignment_list([], Scope) ->
    Scope.

%% Get a list (in order) of all assigned variables

assignments_list(As) when is_list(As) ->
    [Var || {assign,_Ln0,{id,_Ln,Var},_Expr} <- As].

%% Get a set of all assigned variables
assignments(As) ->
    assignments(As, sets:new()).
    
assignments([{assign,_Ln0,{id,_Ln,Var},_Expr}|As], A) ->
    assignments(As, sets:add_element(Var, A));
assignments([], A) ->
    A.

%% Get a set of all parameter names
params(Ps) when is_list(Ps) ->
    params(Ps, sets:new()).

params([{'=',_Ln0,{id,_Ln,Var},_Expr}|Ps], P) ->
    params(Ps, sets:add_element(Var, P));
params([{id,_Ln,Var}|Ps], P) ->
    params(Ps, sets:add_element(Var, P));
params([], P) ->
    P.

%% extract assignments and return them in order of
%% first appearance in the statement list but only keep
%% the last assignment (openScad semantics)

move_assignments({block,Line,Stmts}) ->
    {Stmts1, As} = move_assignments_(Stmts, [], sets:new()),
    {{block,Line,Stmts1}, As};
move_assignments(Stmts) when is_list(Stmts) ->
    move_assignments_(Stmts, [], sets:new()).

%% extract assignments and put the first in statement list
move_assignments_([A={assign,_L1,{id,_L2,Var},_Expr} |Stmts], Acc, As) ->
    As1 = sets:add_element(Var, As),
    {B,Stmts1} = remove_assignments_(Var,A,Stmts,[]),
    move_assignments_(Stmts1, [B|Acc], As1);
%% Is it ok to remove free blocks?
move_assignments_([{block,_Ln,Stmts}|Stmts1], Acc, As) ->
    move_assignments_(Stmts++Stmts1, Acc, As);
move_assignments_([Stmt|Stmts], Acc, As) ->
    move_assignments_(Stmts, [Stmt|Acc], As);
move_assignments_([], Acc, As) ->
    {lists:reverse(Acc), As}.

%% find the last assignment to variable Var remove other assignments along
%% the way to the last assignment
remove_assignments_(Var, _A, [B={assign,_Ln1,{id,_L2,Var},_Expr}|Stmts], Acc) ->
    remove_assignments_(Var, B, Stmts, Acc);
remove_assignments_(Var, A, [{block,_Ln1,Stmts}|Stmts1], Acc) ->
    remove_assignments_(Var, A, Stmts++Stmts1, Acc);
remove_assignments_(Var, A, [Stmt|Stmts], Acc) ->
    remove_assignments_(Var, A, Stmts, [Stmt|Acc]);
remove_assignments_(_Var, A, [], Acc) ->
    {A, lists:reverse(Acc)}.

assignment_list(Acc) ->
    [A || {_V,A} <- Acc].

%% extract all variables used in a statement
use(Stmts) when is_list(Stmts) -> use_stmts(Stmts, sets:new());
use(Stmt) -> use_stmt(Stmt, sets:new()).

use_stmts([Stmt|Stmts], Use) ->
    Us1 = use_stmt(Stmt, Use),
    use_stmts(Stmts, Us1);
use_stmts([], Use) ->
    Use.

use_stmt(Stmt, Use) ->
    case Stmt of
	empty -> Use;
	{block,_Line,Stmts} -> use_stmts(Stmts, Use);
	{module,_Line,{id,_Ln,_Name},Params,Stmt1} ->
	    {Stmt2,As} = move_assignments(Stmt1),
	    {Use1,Ps} = use_params(Params, sets:new()),
	    Use2 = use_stmt(Stmt2, Use1),
	    Locals = sets:union(Ps, As),
	    sets:union(Use, sets:subtract(Use2, Locals));
	{function,_Line,{id,_Ln,_Name},Params,Expr} ->
	    {Use1,Ps} = use_params(Params, sets:new()),
	    Use2 = use_expr(Expr, Use1),
	    sets:union(Use,sets:subtract(Use2, Ps));
	{assign,_Line,{id,_Ln,_Var},Expr} -> 
	    use_expr(Expr, Use);
	{mcall,_Line,_Tags,{id,_Ln,"let"},Args,Stmt1} ->
	    {Use1,As} = use_let_args(Args, sets:new()),
	    Use2 = use_stmt(Stmt1, Use1),
	    sets:union(Use, sets:subtract(Use2, As));
	%% assign is deprecated but used
	{mcall,_Line,_Tags,{id,_Ln,"assign"},Args,Stmt1} ->
	    {Use1,As} = use_let_args(Args, sets:new()),
	    Use2 = use_stmt(Stmt1, Use1),
	    sets:union(Use, sets:subtract(Use2, As));
	{mcall,_Line,_Tags,{id,_Ln,_ID},Args,Stmt1} ->
	    {Use1,_As} = use_args(Args, Use),
	    use_stmt(Stmt1, Use1);
	{'if', _Line, Cond, Then, Else} ->
	    Use1 = use_expr(Cond, Use),
	    Use2 = use_stmt(Then, Use1),
	    use_stmt(Else, Use2);
	{'if', _Line, Cond, Then} ->
	    Use1 = use_expr(Cond, Use),
	    use_stmt(Then, Use1)
    end.

use_expr(Exprs=[_|_], Use) -> use_exprs(Exprs, Use);
use_expr(Expr, Use) ->
    case Expr of
	empty -> Use;
	{vector,_Ln,Elems} -> use_exprs(Elems, Use);
	{id,_Ln,Var} -> sets:add_element(Var, Use);
	{number,_Ln,_Num} -> Use;
	{string,_Ln,_Str} -> Use;
	{true,_Ln} -> Use;
	{false,_Ln} -> Use;
	{undef,_Ln} -> Use;
	{range,_Ln,Start,End} -> use_exprs([Start,End], Use);
	{range,_Ln,Start,Inc,End} -> use_exprs([Start,Inc,End], Use);
	{function,_Ln,Params,Expr} -> 
	    {Use1,Ps} = use_params(Params, sets:new()),
	    Use2 = use_expr(Expr, Use1),
	    sets:union(Use, sets:subtract(Use2, Ps));
	{'let',_Ln,Args,Expr1} -> 
	    {Use1,As} = use_let_args(Args, sets:new()),
	    Use2 = use_expr(Expr1, Use1),
	    sets:union(Use, sets:subtract(Use2, As));
	{'echo',_Ln,Args,Expr} -> use_exprs([Args,Expr], Use);
	{'assert',_Ln,Args,Expr} -> use_exprs([Args,Expr], Use);
	{op,_Ln,call,Call,Args} ->
	    {Use1,_As} = use_args(Args, Use),use_exprs(Call, Use1);
	{op,_Ln,_Op,Arg1} -> use_expr(Arg1, Use);
	{op,_Ln,_Op,Arg1,Arg2} -> use_exprs([Arg1,Arg2],Use);
	{op,_Ln,'?',Cond,Then,Else} -> use_exprs([Cond,Then,Else], Use);
	%% list comprehension
	{lc_for,_Ln,Args,Expr} -> 
	    {Use1,As} = use_args(Args, sets:new()),
	    Use2 = use_expr(Expr, Use1),
	    sets:union(Use, sets:subtract(Use2, As));
	{lc_forc,_Ln,Init,Cond,Update,Element1} ->
	    {Use1, As} = use_args(Init, sets:new()),
	    Use2 = use_expr(Cond, Use1),
	    {Use3, _As2} = use_args(Update, Use2), %%? new variables allowed?
	    Use4 = use_expr(Element1, Use3),
	    sets:union(Use, sets:subtract(Use4, As));
	{lc_each,_Ln,Element1} -> 
	    use_expr(Element1, Use);
	{lc_let,_Ln,Args,Element1} -> 
	    {Use1, As} = use_args(Args, sets:new()),
	    Use2 = use_expr(Element1, Use1),
	    sets:union(Use, sets:subtract(Use2, As));
	{lc_if,_Ln,Cond,Then} ->
	    Use1 = use_expr(Cond, Use),
	    use_expr(Then, Use1);
	{lc_if,_Ln,Cond,Then,Else} ->
	    Use1 = use_expr(Cond, Use),
	    Use2 = use_expr(Then, Use1),
	    use_expr(Else, Use2)
    end.

use_exprs([Expr|Exprs], Use) ->
    use_exprs(Exprs, use_expr(Expr, Use));
use_exprs([], Use) ->
	Use.

use_params(Params, Use) ->
    use_params(Params, sets:new(), Use).
use_params([{'=',_Ln0,{id,_Ln,Var},Expr}|Args], Ps, Use) ->
    use_params(Args, sets:add_element(Var, Ps), use_expr(Expr, Use));
use_params([{id,_Ln,Var}|Args], Ps, Use) ->
    use_params(Args, sets:add_element(Var, Ps), Use);
use_params([], Ps, Use) ->
    {Use, Ps}.


use_args(Params, Use) ->
    use_args(Params, sets:new(), Use).
use_args([{'=',_Ln0,{id,_Ln,Var},Expr}|Args], As, Use) ->
    use_args(Args, sets:add_element(Var, As), use_expr(Expr, Use));
use_args([Expr|Args], As, Use) ->
    use_args(Args, As, use_expr(Expr, Use));
use_args([], As, Use) ->
    {Use, As}.


%% sequential bind of
use_let_args(Args, Use) ->
    use_let_args(Args, sets:new(), Use).

use_let_args([{'=',_Ln0,{id,_Ln,Var},Expr}|Args], As, Use) ->
    Use1 = use_expr(Expr, Use),
    use_let_args(Args, sets:add_element(Var, As), Use1);
use_let_args([], As, Use) ->
    {Use, As}.



%% [!#%*] ID(Args) { Stmt1; Stmt2; ... }
eval_mcall(ID, Args, _Tags, ChildStmt, Scope0) ->
    Ms = maps:get(modules,Scope0,[]),
    Parents = maps:get(parent_modules,Scope0,[]),
    case maps:get(ID, Ms, undef) of
	{_Ps,ModFun} when is_function(ModFun) ->
	    ModFun(Args, ChildStmt, Scope0);
	{Bound,Params,Stmt} ->
	    Values = eval_arg_list(Args, Scope0),
	    Scope1 = set(Bound, Scope0),  %% update scope with bound variables
	    Ps = eval_params(Params, Scope1),
	    %% io:format("mcall ~p,bound = ~p, ps=~p, values=~p\n",
	    %% [ID,Bound,Ps,Values]),
	    Scope2 = bind(Ps, Values, Scope1),
	    Children = case eval_child_(ChildStmt, Scope0) of
			   L=[_|_] -> L;
			   H -> [H]
		       end,
	    Len = length(Children),
	    Parents1 = [ID|Parents],
	    {Obj,_} = eval_stmt(Stmt, Scope2#{"$children" => Len,
					      children=>Children,
					      parent_modules=>Parents1,
					      "$parent_modules"=>Parents1}),
	    Obj
    end.

eval_for(Var, Range, Child, Scope) ->
    case Range of
	{range,{Start,Inc,End}} ->
	    Rs =
		fold_sequence(Start, End, Inc,
			      fun(Val,Acc) ->
				      Scope1 = set(Var,Val, Scope),
				      {Obj,_} = eval_stmt(Child,Scope1),
				      [Obj|Acc]
			      end, []),
	    lists:reverse(Rs);
	Vec when is_list(Vec) ->
	    Rs =
		lists:foldl(fun(Val,Acc) ->
				    Scope1 = set(Var,Val, Scope),
				    {Obj,_} = eval_stmt(Child,Scope1),
				    [Obj|Acc]
			    end, []),
	    lists:reverse(Rs)
    end.


eval_child(empty, Op, _Scope) ->
    Op;
eval_child(Child, Color=#color{}, Scope) ->
    eval_child_(Child, Scope#{color=>Color});
eval_child(Child, T=#transform{}, Scope) ->
    Ts = maps:get(transform, Scope, []),
    eval_child_(Child, Scope#{transform=>[T|Ts]});
eval_child(Child, undefined, Scope) ->
    eval_child_(Child, Scope).

eval_child_({block,_Line,Stmts}, Scope) -> %% scope!
    {Stmts1,_As} = move_assignments(Stmts),
    {Obj,_} = eval_stmt_list(Stmts1, Scope),
    Obj;
eval_child_(Stmt, Scope) -> 
    {Obj,_} = eval_stmt(Stmt, Scope),
    Obj.


builtin_functions() ->
    #{
      "version" => {[], fun([]) -> [2011,9,23] end},
      "version_num" => {[], fun([]) -> 20110923 end},
      "cos" => {["x"], fun([X]) -> math:cos(deg2rad(X)) end},
      "sin" => {["x"], fun([X]) -> math:cos(deg2rad(X)) end},
      "tan" => {["x"], fun([X]) -> math:tan(deg2rad(X)) end},
      "acos" => {["x"], fun([X]) -> math:acos(deg2rad(X)) end},
      "asin" => {["x"], fun([X]) -> math:asin(deg2rad(X)) end},
      "atan" => {["x"], fun([X]) -> math:atan(deg2rad(X)) end},
      "atan2" => {["y","x"], fun([Y,X]) -> math:atan2(deg2rad(Y),deg2rad(X)) end},
      "abs" => {["x"], fun([X]) -> abs(X) end},
      "ceil" => {["x"], fun([X]) -> trunc(math:ceil(X)) end},
      "concat" => {["*"], fun(X) -> lists:flatten(X) end},
      "cross" => {["a","b"], fun([A,B]) -> cross(A,B) end},
      "exp" => {["x"], fun([X]) -> math:exp(X) end},
      "floor" => {["x"], fun([X]) -> trunc(math:floor(X)) end},
      "ln" => {["x"], fun([X]) -> math:log(X) end},
      "len" => {["a"], fun([A]) when is_binary(A) -> byte_size(A); %%fixme
			  ([A]) when is_list(A) -> length(A)
		       end},
      "log" => {["x"], fun([X]) -> math:log10(X) end},
      "lookup" => {["k","kv"], fun([K,KV]) -> lookup(K,KV) end},
      "max" => {["*"], fun([V]) when is_list(V) -> lists:max(V);
			  ([X,Y]) when is_number(X), is_number(Y) -> max(X,Y);
			  (Xs) -> lists:max(Xs)
		       end},
      "min" => {["*"], fun([V]) when is_list(V) -> lists:min(V);
			  ([X,Y]) when is_number(X), is_number(Y) -> min(X,Y);
			  (Xs) -> lists:min(Xs)
		       end},
      "norm" => {["v"], fun([V]) -> norm(V) end},
      "pow" => {["x","y"], fun([X,Y]) -> math:pow(X,Y) end},
      "rands" => {["min_value","max_value", "value_count", "seed_value"],
		  fun([A,B,N]) when is_number(A), is_number(B), A =< B,
				    is_integer(N), N >= 0 ->
			  D = B-A,
			  [A+D*rand:uniform() || _ <- lists:seq(1,N)];
		     ([A,B,N,Seed]) when is_number(A), is_number(B), A =< B,
					 is_integer(N), N >= 0, 
					 is_integer(Seed) ->
			  rand:seed(exsss,Seed),
			  D = B-A,
			  [A+D*rand:uniform() || _ <- lists:seq(1,N)]
		  end},
      "round" => {["x"], fun([X]) -> round(X) end},
      "sign" => {["x"], fun([X]) when X > 0 -> 1;
			   ([X]) when X < 0 -> -1;
			   ([_]) -> 0
			end},
      "sqrt" => {["x"], fun([X]) -> math:sqrt(X) end},
      "parent_module" => {["n"], 
			  fun([N],Scope) ->
				  Ps = maps:get(parent_modules,Scope,[]),
				  lists:nth(N+1, Ps)
			  end},
      %% "str" => {["x"], fun([X]) -> io_lib:format("~p", [X]) end},
      %% "chr" => {["x"], fun([X]) -> [X] end},
      %% "ord" => {["x"], fun([X]) -> hd(X) end},
      "is_string" => {["x"], fun([X]) -> is_binary(X) end},
      "is_list" => {["x"], fun([X]) -> is_list(X) end},
      "is_num" => {["x"], fun([X]) -> is_number(X) end},
      "is_bool" => {["x"], fun([X]) -> is_boolean(X) end},
      "is_undef" => {["x"], fun([X]) -> X == undef end},
      "is_function" => {["x"], fun([X]) -> is_function(X) end}
      %% "search" => ...
      %% FIXME: add more functions (ALL)
     }.

builtin_modules() ->
    #{
     %% 3D objects
      "cylinder" => 
	  {["h","r","r1","r2","center","d","d1","d2", "$fa","$fs","$fn"],
	   fun mod_cylinder/3},
      "sphere" => {["r","d", "$fa", "$fs", "$fn"], fun mod_sphere/3},
      "cube" => {["size", "center"], fun mod_cube/3},
      %% Color
      "color"  => {["c", "alpha"], fun mod_color/3},
      %% Transformations
      "translate" => {["v"], fun mod_translate/3},
      "rotate"    => {["a","v"], fun mod_rotate/3},
      "scale"  => {["v"], fun mod_scale/3},
      %% Operations
      %% "assert" => {["*"], fun mod_assert/3},
      "echo" => {["*"], fun mod_echo/3},
      %% "each" => { ["*"], fun mod_each/3},

      "children" => {["*"], fun mod_children/3},
      "intersection" => {["*"], fun mod_intersection/3},
      "union" => {["*"], fun mod_union/3},
      "difference" => {["*"], fun mod_difference/3}
      %% ADD more operations/objects and transformations
     }.

default_scope(Filename) ->
    #{
      transform => [],
      functions => builtin_functions(),
      modules => builtin_modules(),
      filename => Filename,
      "$fa" => 12,  %% is the minimum angle for a fragment (>= 0.01)
      "$fs" => 2,   %% the minimum size of a fragment (>= 0.01)
      "$fn" => 0,  %% the number of fragments
      "$t" => 0,   %% animation steps... (fimxe)
      "$vpr" => 0, %% viewport rotation
      "$vpt" => 0, %% viewport translation
      "$vpf" => 0, %% viewport FOV
      "$vpd" => 0, %% viewport camera distance
      "$preview" => false
     }.

eval_expr(Expr, Scope) ->
    try eval_expr_(Expr, Scope) of
	Value -> Value
    catch
	error:_ ->
	    %% io:format("eval_expr: failed ~p~n", [Expr]),
	    undef
    end.


eval_expr_(empty, _Scope) ->
    true;
eval_expr_({vector,_ln,Elems}, Scope) ->
    eval_vector(Elems, Scope);
eval_expr_({id,_Ln,Var}, Scope) ->
    %% should we override functions?
    case maps:get(Var, Scope, undef) of
	undef ->
	    Fs = maps:get(functions, Scope, #{}),
	    %%io:format("Var = ~p\nFs=~p\n", [Var,Fs]),
	    case maps:get(Var, Fs, undef) of
		{_Ps,Fun} when is_function(Fun) ->
		    %%io:format("value: ~s = ~p\n", [Var, Fun]),
		    Fun;
		{Ps,Expr} ->
		    %%io:format("value: ~s (~p) = ~p\n", [Var, Ps, Expr]),
		    fun(Args) ->
			    Scope1 = bind(Ps, Args, Scope),
			    eval_expr_(Expr, Scope1)
		    end;
		undef ->
		    io:format("undefined variable ~p~n", [Var]),
		    undef
	    end;
	Val -> 
	    %%io:format("value: ~s = ~p\n", [Var, Val]),
	    Val
    end;
eval_expr_({number,_Ln,Num}, _Scope) -> Num;
eval_expr_({string,_Ln,Str}, _Scope) -> Str;
eval_expr_({true,_Ln}, _Scope) ->  true;
eval_expr_({false,_Ln}, _Scope) -> false;
eval_expr_({undef,_Ln}, _Scope) -> undef;
eval_expr_({range,_Ln,Start,End}, Scope) ->
    S = eval_expr_(Start, Scope),
    E = eval_expr_(End, Scope),
    {range, {S, 1, E}};
eval_expr_({range,_Ln,Start,Inc,End}, Scope) ->
    S = eval_expr_(Start, Scope),
    I = eval_expr_(Inc, Scope),
    E = eval_expr_(End, Scope),
    {range, {S, I, E}};
eval_expr_({function,_Ln,Params,Expr}, Scope) ->
    Ps = eval_params(Params,Scope),
    fun(Vs) ->
	    Scope1 = bind(Ps, Vs, Scope),
	    eval_expr(Expr, Scope1)
    end;
eval_expr_({'echo',_Ln,Args,Expr}, Scope) ->
    As = [eval_expr_(A, Scope) || A <- Args],
    echo(As),
    eval_expr_(Expr, Scope);
eval_expr_({'assert',Ln,Args,Expr}, Scope) ->
    As = [eval_expr(A, Scope) || A <- Args],
    assert(As,Args,Ln,Scope),
    eval_expr(Expr, Scope);
eval_expr_({'let',_Ln,Args,Expr}, Scope) ->
    Scope1 = eval_let_args(Args, Scope),
    eval_expr_(Expr, Scope1);
eval_expr_({op,_Ln,call,Call,Args},Scope) -> 
    Call1 = eval_expr_(Call, Scope),
    if is_function(Call1, 1) ->
	    Args1 = eval_arg_list(Args, Scope),
	    Call1(Args1);
       is_function(Call1, 2) ->
	    Args1 = eval_arg_list(Args, Scope),
	    Call1(Args1, Scope);
       true ->
	    io:format("~p is not a function~n", [Call1]),
	    undef
    end;
eval_expr_({op,_Ln,Op,Arg1},Scope) -> 
    A1 = eval_expr_(Arg1, Scope),
    case Op of
	'+' -> A1;
	'-' -> negate(A1);
	'!' -> not bool(A1);
	_ -> undef
    end;
eval_expr_({op,_Ln,Op,Arg1,Arg2},Scope) ->
    A1 = eval_expr_(Arg1, Scope),
    A2 = eval_expr_(Arg2, Scope),
    case Op of
	'+' -> add(A1,A2);
	'-' -> subtract(A1,A2);
	'*' -> multiply(A1, A2);
	'/' -> divide(A1, A2);
	'%' -> reminder(A1, A2);
	'==' -> A1 == A2;
	'!=' -> A1 /= A2;
	'>' -> A1 > A2;
	'>=' -> A1 >= A2;
	'<' -> A1 < A2;
	'<=' -> A1 =< A2;
	'&&' -> bool(A1) andalso bool(A2);
	'||' -> bool(A1) orelse bool(A2);
	'^' -> math:pow(A1,A2);
	index -> lists:nth(A1+1,A2);
	member ->
	    case A1 of
		<<"x">> -> lists:nth(1,A2);
		<<"y">> -> lists:nth(2,A2);
		<<"z">> -> lists:nth(3,A2)
	    end;
	_ -> undef
    end;
eval_expr_({op,_Ln,'?',Cond,Then,Else},Scope) ->
    C = eval_expr_(Cond,Scope),
    case bool(C) of
	true -> eval_expr_(Then, Scope);
	false -> eval_expr_(Else, Scope)
    end.


eval_vector([Element|Es], Scope) ->
    eval_vector_element(Element, Scope) ++ eval_vector(Es, Scope);
eval_vector([], _Scope) ->
    [].

eval_vector_element(Element, Scope) ->
    case Element of
	{lc_for,_Ln,Args,Element1} ->
	    case eval_arg_list(Args, Scope) of %% Fixme: multiple Args...
		[{Var,Range}|_] ->
		    eval_lc_for(Var, Range, Element1, Scope)
	    end;

	{lc_let,_Ln,Args,Element1} ->
	    Scope1 = eval_let_args(Args, Scope),
	    eval_vector_element(Element1, Scope1);

	{lc_each,_Ln, Element1} ->
	    eval_vector_element(Element1, Scope);

	{lc_forc,_Ln,Init,Cond,Update,Element1} ->
	    Scope1 = eval_let_args(Init, Scope),
	    eval_lc_forc(Cond, Update, Element1, Scope1);

	{lc_if,_Ln,Cond,Then} ->
	    case bool(eval_expr(Cond, Scope)) of
		true -> eval_vector_element(Then, Scope);
		false -> []
	    end;
	    
	{lc_if,_Ln,Cond,Then,Else} ->
	    case bool(eval_expr(Cond, Scope)) of
		true -> eval_vector_element(Then, Scope);
		false -> eval_vector_element(Else, Scope)
	    end;
	_ ->
	    [eval_expr(Element, Scope)]
    end.


eval_lc_forc(Cond, Update, Element, Scope) ->
    case bool(eval_expr(Cond, Scope)) of
	true ->
	    Value = eval_vector_element(Element, Scope),
	    Scope1 = eval_let_args(Update, Scope),
	    [Value | eval_lc_forc(Cond, Update, Element, Scope1)];
	false ->
	    []
    end.

eval_lc_for(Var, Range, Expr, Scope) ->
    case Range of
	{range,{Start,Inc,End}} ->
	    Rs =
		fold_sequence(Start, End, Inc,
			      fun(Val,Acc) ->
				      Scope1 = set(Var,Val, Scope),
				      [eval_expr(Expr, Scope1)|Acc]
			      end, []),
	    lists:reverse(Rs);
	Vec when is_list(Vec) ->
	    Rs =
		lists:foldl(fun(Val,Acc) ->
				    Scope1 = set(Var,Val, Scope),
				    [eval_expr(Expr, Scope1)|Acc]
			    end, []),
	    lists:reverse(Rs)
    end.



%% sequential bind of
eval_let_args([{'=',_Ln0,{id,_Ln,ID},Arg}|Args], Scope) ->
    Val = eval_expr(Arg, Scope),
    eval_let_args(Args, set(ID,Val,Scope));
eval_let_args([], Scope) ->
    Scope.

eval_arg_list([{'=',_Ln0,{id,_Ln,ID},Arg}|Args], Scope) ->
    Val = eval_expr(Arg, Scope),
    [{ID,Val}|eval_arg_list(Args, Scope)];
eval_arg_list([Arg|Args], Scope) ->
    Val = eval_expr(Arg, Scope),
    [Val|eval_arg_list(Args, Scope)];
eval_arg_list([], _Scope) ->
    [].

eval_arg_list([{'=',_Ln0,{id,_Ln,ID},Expr}|Args],[_|Order],Scope) ->
    Val = eval_expr(Expr, Scope),
    [{ID,Val}|eval_arg_list(Args, Order, Scope)];
eval_arg_list([Arg|Args], [ID|Order], Scope) ->
    Val = eval_expr(Arg, Scope),
    [{ID,Val}|eval_arg_list(Args, Order, Scope)];
eval_arg_list([], [ID=[$$|_]|Order], Scope) -> 
    case maps:get(ID, Scope, undef) of
	undef -> eval_arg_list([], Order, Scope);
	Val -> [{ID,Val}|eval_arg_list([], Order, Scope)]
    end;
eval_arg_list([], [_ID|Order], Scope) ->
    eval_arg_list([], Order, Scope);
eval_arg_list([], [], _Scope) -> 
    [];
eval_arg_list(Rest, [], _Scope) -> 
    [{rest,Rest}].

eval_params([{'=',_Ln0,{id,_Ln,ID},Expr}|Args], Scope) ->
    Val = eval_expr(Expr, Scope),
    [{ID,Val}|eval_params(Args, Scope)];
eval_params([{id,_Ln,ID}|Args], Scope) ->
    [ID | eval_params(Args, Scope)];
eval_params([], _Scope) ->
    [].

%% Extract value from variables in Vs
get_vars(Vs, Scope) when is_list(Vs) ->
    [{V,maps:get(V, Scope, undef)} || V <- Vs];
get_vars(Vs, Scope) -> %% assume set!
    get_vars(sets:to_list(Vs), Scope).
    


add(A,B) when is_number(A), is_number(B) -> A + B;
add([A|As],[B|Bs]) -> [add(A,B)|add(As,Bs)];
add([], _) -> [];
add(_, []) -> [].

subtract(A,B) when is_number(A), is_number(B) -> A - B;
subtract([A|As],[B|Bs]) -> [subtract(A,B)|subtract(As,Bs)];
subtract([], _) -> [];
subtract(_, []) -> [].

negate(A) when is_number(A) -> -A;
negate([A|As]) -> [negate(A)|negate(As)];
negate([]) -> [].

multiply(A,B) when is_number(A), is_number(B) -> A * B;
multiply(A,Bs) when is_number(A), is_list(Bs) -> scale_(A, Bs);
multiply(As,B) when is_list(As), is_number(B) -> scale_(B, As);
multiply(As,Bs) when is_list(As), is_number(hd(As)),
		     is_list(Bs), is_number(hd(Bs)) ->
    dot_(As, Bs);
multiply(As,Bs) when is_list(As), is_list(hd(As)),
		     is_list(Bs), is_number(hd(Bs)) ->
    Cs = matmul_(As, [Bs]),
    [C || [C] <- Cs];
multiply(As,Bs) when is_list(As), is_number(hd(As)),
		is_list(Bs), is_list(hd(Bs)) ->
    matmul_([As], transpose(Bs));
multiply(As,Bs) when is_list(As), is_list(hd(As)),
		     is_list(Bs), is_list(hd(Bs)) ->
    matmul_(As, transpose(Bs)).

reminder(A,B) when is_integer(A), is_integer(B), B > 0 ->
    if A rem B  =:= 0 -> A div B;
       true -> A / B
    end;
reminder(A,B) when is_number(A), is_number(B) -> 
    math:fmod(A,B).

divide(A,B) when is_integer(A), is_integer(B), B > 0 ->
    if A rem B  =:= 0 -> A div B;
       true -> A / B
    end;
divide(A,B) when is_number(A), is_number(B) -> A / B;
divide(A,Bs) when is_number(A), is_list(Bs) -> iscale_(A, Bs);
divide(As,B) when is_list(As), is_number(B) -> scale_(1/B, As).

scale(A, B) when is_number(A), is_number(B) -> 
    A * B;
scale(A,Bs) when is_number(A), is_list(Bs) -> 
    scale_(A, Bs);
scale(As,B) when is_list(As), is_number(B)  -> 
    scale_(B, As).

scale_(A, [B|Bs]) when is_number(B) -> [A*B|scale_(A, Bs)];
scale_(A, [B|Bs]) when is_list(B) -> [scale_(A,B)|scale_(A,Bs)];
scale_(_A, []) -> [].

iscale_(A, [B|Bs]) when is_number(B) -> [A/B|iscale_(A, Bs)];
iscale_(A, [B|Bs]) when is_list(B) -> [iscale_(A,B)|iscale_(A,Bs)];
iscale_(_A, []) -> [].

dot_([A|As], [B|Bs]) when is_number(A), is_number(B) ->
    A*B + dot_(As, Bs);
dot_([], []) -> 0.

norm([]) -> 0;
norm(As) when is_list(As), is_number(hd(As))  ->
    math:sqrt(lists:sum([A*A || A <- As])).

cross([A1,A2], [B1,B2]) ->
    [A1*B2 - A2*B1];
cross([A1,A2,A3], [B1,B2,B3]) ->
    [A2*B3 - A3*B2, A3*B1 - A1*B3, A1*B2 - A2*B1].

lookup(K, [[K1,V1]|_]) when K =< K1 -> V1;
lookup(K, [[K1,V1]|KVs]) -> lookup_(K, K1, V1, KVs).
    
lookup_(K, _K0, _V0, [[K1,V1]]) when K > K1 -> V1;
lookup_(K,  K0, V0, [[K1,V1]|_]) when K =< K1 ->
    Kd = K1 - K0,
    Vd = V1 - V0,
    V0 + Vd * (K - K0) / Kd;
lookup_(K, _K0, _V0, [[K1,V1]|KVs]) when K > K1 ->
    lookup_(K, K1, V1, KVs).


matmul(As,Bs) when is_list(As), is_list(hd(As)),
		   is_list(Bs), is_list(hd(Bs)) ->
    matmul_(As, transpose(Bs)).

matmul_([A|As], Bs) when is_list(A) ->
    [ [dot_(A, B) || B <- Bs] | matmul_(As, Bs)];
matmul_([], _) -> [].

transpose([A|As]) when is_list(A) ->
    [ [lists:nth(N, V) || V <- [A|As]] || N <- lists:seq(1,length(A))].

%% fixme: understand the binding rules 
bind([{Var,Default}|Ps], Vs, Scope) ->
    Value = proplists:get_value(Var, Vs, Default),
    bind(Ps, Vs, set(Var,Value,Scope));
bind([Var|Ps], [Val|Vs], Scope) when is_list(Var) ->
    bind(Ps, Vs, set(Var,Val,Scope));
bind([], _, Scope) ->
    Scope.


%% set a variable to a value in a scope
set(Name, Value, Scope) ->
    %%io:format("set: ~s = ~p~n", [Name,Value]),
    Scope#{ Name => Value }.

%% bind a list of variables to values in a scope
set([{Name,Value}|Vs], Scope) ->
    set(Vs, set(Name,Value,Scope));
set([], Scope) ->
    Scope.

bool(false) -> false;
bool(Num) when is_number(Num) -> not (Num == 0);
bool(<<>>) -> false;  %% empty string
bool([]) -> false;    %% empty vec
bool(undef) -> false;
bool(_) -> true.
	    
rad2deg(Rad) ->
    Rad * 180 / math:pi().
deg2rad(Deg) ->
    Deg * math:pi() / 180.	

echo(As) ->
    Data =
	lists:join(",",
		   [case A of
			{ID,Value} ->
			    [ID," = ", format_value(Value)];
			Value ->
			    format_value(Value)
		    end || A <- As]),
    io:format("// ECHO: ~s\n", [Data]).

format_value(Int) when is_integer(Int) ->   integer_to_list(Int);
format_value(Float) when is_float(Float) -> io_lib:format("~.6e", [Float]);
format_value(Str) when is_binary(Str) -> 
    [$", unicode:characters_to_list(Str), $"];
format_value(Atom) when is_atom(Atom) -> 
    atom_to_list(Atom);
format_value(Fun) when is_function(Fun) ->
    "#function";
format_value(Vec) when is_list(Vec) -> %% vector
    ["[",lists:join(",", [format_value(V) || V <- Vec]), "]"];
format_value({range,{S,I,E}}) -> 
    ["[",format_value(S),":",format_value(I),":",format_value(E),"]"].

assert([Cond],[Expr|_],Line,Scope) ->
    if not Cond ->
	    Filename = maps:get(filename, Scope, "unknown"),
	    io:format("// ERROR: Assertion '~p' failed in file ~s, line ~p\n", 
		      [Expr,Filename,Line]),
	    io:format("scope: ~p\n", [Scope]),
	    throw(assertion_failed);
       true ->
	    true
    end;
assert([Cond,Message],[Expr|_],Line,Scope) ->
    if not Cond ->
	    Filename = maps:get(filename, Scope, "unknown"),
	    io:format("// ERROR: Assertion '~p' ~p, failed in file ~s, line ~p\n", 
		      [Expr,Message,Filename,Line]),
	    io:format("scope: ~p\n", [Scope]),
	    throw(assertion_failed);
       true ->
	    true
    end.    

%% color props [{c,Color}, {alpha,Alpha}] to
%% {R,G,B,A} range 0..1
color(Props) ->
    A0 = proplists:get_value(alpha, Props, undef),
    C0 = proplists:get_value(c, Props, undef),
    case C0 of
	undef -> make_color([0.0,0.0,0.0], A0);
	Color when is_list(Color) -> make_color(Color,A0);
	<<"#",R,G,B>> -> %% #rgb
	    R4 = list_to_integer([R], 16),
	    G4 = list_to_integer([G], 16),
	    B4 = list_to_integer([B], 16),
	    make_color([R4/16,G4/16,B4/16], A0);
	<<"#",R,G,B,A>> -> %% #rgba
	    R4 = list_to_integer([R], 16),
	    G4 = list_to_integer([G], 16),
	    B4 = list_to_integer([B], 16),
	    A4 = list_to_integer([A], 16),
	    make_color([R4/16,G4/16,B4/16,A4/16], A0);
	<<"#",R1,R2,G1,G2,B1,B2>> -> %% #rrggbb
	    R8 = list_to_integer([R1,R2], 16),
	    G8 = list_to_integer([G1,G2], 16),
	    B8 = list_to_integer([B1,B2], 16),
	    make_color([R8/255,G8/255,B8/255], A0);
	<<"#",R1,R2,G1,G2,B1,B2,A1,A2>> -> %% #rrggbb
	    R8 = list_to_integer([R1,R2], 16),
	    G8 = list_to_integer([G1,G2], 16),
	    B8 = list_to_integer([B1,B2], 16),
	    A8 = list_to_integer([A1,A2], 16),
	    make_color([R8/255,G8/255,B8/255,A8/255], A0);
	ColorName when is_binary(ColorName) ->
	    {R,G,B} = epx_color:from_name(binary_to_list(ColorName)),
	    make_color([R/255,G/255,B/255], A0)
    end.

make_color([R,G,B], undef) -> [R,G,B,1.0];
make_color([R,G,B], A)     -> [R,G,B,A];
make_color(Color=[_R,_G,_B,_A], undef) -> Color;
make_color([R,G,B,_], A)     -> [R,G,B,A].


sequence(Start, Stop, Step) when is_number(Start), is_number(Stop), 
				 is_number(Step),
				 Start =< Stop, Step > 0 ->
    map_sequence(Start, Stop, Step, fun(X) -> X end);
sequence(_, _, _) ->
    [].

map_sequence(Start, Stop, Fun) ->
    map_sequence(Start, Stop, 1, Fun).

map_sequence(Start, Stop, Step, Fun) ->
    lists:reverse(fold_sequence(Start, Stop, Step, fun(X,Acc) -> [Fun(X)|Acc] end, [])).


fold_sequence(Start, Stop, Fun, Acc) ->
    fold_sequence(Start, Stop, 1, Fun, Acc).

fold_sequence(Start, Stop, Step, Fun, Acc) 
  when is_function(Fun, 2),
       is_number(Start), is_number(Stop), 
       is_number(Step),
       Start =< Stop, Step > 0 ->
    fold_sequence_(Start, Stop, Step, Fun, Acc);
fold_sequence(_, _, _, _Fun, Acc) ->
    Acc.

fold_sequence_(Pos, End, Step, Fun, Acc) when Pos =< End ->
    Acc1 = Fun(Pos, Acc),
    fold_sequence_(Pos+Step, End, Step, Fun, Acc1);
fold_sequence_(_, _, _, _Fun, Acc) ->
    Acc.

%%
%% built in modules - move to various files?
%%

mod_union(Args, Child, Scope) ->
    Order = [],
    _Prop = eval_arg_list(Args, Order, Scope),
    Children = eval_child(Child, undefined, Scope),
    Union = #object{type=union, children=Children},
    set_mod(Union, Scope).
    

mod_difference(Args, Child, Scope) ->
    Order = [],
    _Prop = eval_arg_list(Args, Order, Scope),
    Children = eval_child(Child, undefined, Scope),
    Difference = #object {type=difference, children=Children},
    set_mod(Difference, Scope).

mod_intersection(Args, Child, Scope) ->
    Order = [],
    _Prop = eval_arg_list(Args, Order, Scope),
    Children = eval_child(Child, undefined, Scope),
    Op = #object {type=intersection, children=Children},
    set_mod(Op, Scope).

mod_echo(Args, _Child, Scope) ->
    As = eval_arg_list(Args, Scope),
    echo(As),
    true.

mod_children([], _Child, Scope) ->
    apply_mods(maps:get(children, Scope), Scope);
mod_children([A], _Child, Scope) ->
    Cs = maps:get(children, Scope),
    case eval_expr(A, Scope) of
	I when is_integer(I) ->
	    C = lists:nth(I+1, Cs),
	    C1 = apply_mod(C, Scope),
	    %%io:format("child(~w) = ~p\n", [I,C1]),
	    C1;
	{range,{S,I,E}} ->
	    List = sequence(S, E, I),
	    lists:map(
	      fun(J) ->
		      C = lists:nth(J+1,Cs),
		      C1 = apply_mod(C, Scope),
		      %%io:format("child(~w) = ~p\n", [J,C1]),
		      C1
	      end, List)
    end.

mod_translate(Args, Child, Scope) ->
    Order = ["v"],
    Props = eval_arg_list(Args, Order, Scope),
    V = proplists:get_value("v", Props, undef),
    T = #transform {type=translate,params=[{v,V}]},
    eval_child(Child, T, Scope).

mod_scale(Args, Child, Scope) ->
    Order = ["v"],
    Props = eval_arg_list(Args, Order, Scope),
    V = proplists:get_value("v", Props, undef),
    T = #transform{type=scale,params=[{v,V}]},
    eval_child(Child, T, Scope).

mod_rotate(Args, Child, Scope) ->
    Order = ["a", "v"],
    Props = eval_arg_list(Args, Order, Scope),
    A = proplists:get_value("a", Props, 0),
    V = proplists:get_value("v", Props, undef),
    T = #transform{type=rotate,params=[{a,A},{v,V}]},
    eval_child(Child, T, Scope).

mod_color(Args, Child, Scope) ->
    Order = ["c", "alpha"],
    Props = eval_arg_list(Args, Order, Scope),
    C = proplists:get_value("c", Props, undef),
    A = proplists:get_value("alpha", Props, undef),
    Color = #color{c=C,alpha=A},
    eval_child(Child, Color, Scope).

mod_cylinder(Args, _Child, Scope) ->
    Order = ["h","r","r1","r2","center","d","d1","d2",
	     "$fa","$fs","$fn"],
    Prop = eval_arg_list(Args, Order, Scope),
    H = proplists:get_value("h", Prop, 1),
    D = proplists:get_value("d", Prop, undef),
    R = proplists:get_value("r", Prop, undef),
    R1 = case proplists:get_value("d1", Prop, undef) of
	     undef when D /= undef -> D/2;
	     undef -> proplists:get_value("r1", Prop, R);
	     D1 -> D1/2
	 end,
    R2 = case proplists:get_value("d2", Prop, undef) of
	     undef when D /= undef -> D/2;
	     undef -> proplists:get_value("r2", Prop, R);
	     D2 -> D2/2
	 end,
    Center = proplists:get_value("center", Prop, false),
    Cylinder = #object{type=cylinder,
		       params=[{h,H},{r1,R1},{r2,R2},{center,Center}]},
    set_mod(Cylinder, Scope).


mod_sphere(Args, _Child, Scope) ->    
    Order = ["r","d", "$fa", "$fs", "$fn"],
    Res = eval_arg_list(Args, Order, Scope),
    R = case proplists:get_value("d", Res, undef) of
	    undef -> proplists:get_value("r", Res, 1);
	    D -> D/2
	end,
    Sphere = #object{type=sphere,params=[{r,R}] },
    set_mod(Sphere, Scope).

mod_cube(Args, _Child, Scope) ->
    Order = ["size","center"],
    Props = eval_arg_list(Args, Order, Scope),
    Size = proplists:get_value("size", Props, [1,1,1]),
    Center = proplists:get_value("center", Props, false),
    Cube = #object{type=cube, params=[{size,Size},{center,Center}]},
    set_mod(Cube, Scope).


set_mods([Obj|Objs], Scope) ->
    [set_mod(Obj, Scope)|set_mods(Objs, Scope)];
set_mods([], _Scope) ->
    [].

set_mod(Obj=#object{color=C}, Scope) ->
    Obj#object{transforms = maps:get(transform, Scope, []), 
	       color = maps:get(color, Scope, C)}.

apply_mods([C|Cs], Scope) ->
    [apply_mod(C, Scope)|apply_mods(Cs, Scope)];
apply_mods([], _Scope) ->
    [].

apply_mod(Obj=#object{transforms=Ts1, color=C1}, Scope) ->
    Ts = maps:get(transform, Scope, [])++Ts1,
    C  = maps:get(color, Scope, C1),
    Obj#object{transforms=Ts, color=C};
apply_mod(Other, _Scope) -> %% true
    Other.


