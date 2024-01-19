%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    wings plugin to import OpenSCAD files.
%%% @end
%%% Created : 18 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(wpc_scad).

-compile([export_all]).

import(FileName) ->
    {ok,Ast} = scad:parse_file(FileName),
    scad_import(Ast).

%% be inspired by wings/plugins_src/import_export/x3d_import.erl

scad_import(Ast) ->
    %% setup default variable values "$fn" etc
    Scope = default_scope(),
    eval_stmt_list(Ast, Scope).

eval_stmt_list(Ast, Scope) ->
    eval_stmt_list(Ast, Scope, []).
eval_stmt_list([Stmt|Stmts], Scope, Acc) ->
    {A,Scope1} = eval_stmt(Stmt, Scope),
    eval_stmt_list(Stmts, Scope1, [A|Acc]);
eval_stmt_list([], Scope, Acc) ->
    {lists:reverse(Acc), Scope}.

eval_stmt(Stmt, Scope) ->
    case Stmt of
	empty ->
	    {[], Scope};  %% empty?
	{block,_Line,Stmts} -> %% not a scope !?
	    eval_stmt_list(Stmts, Scope);
	{module,_Line,{id,_,ID},Params,Statement} ->
	    Ps = eval_params(Params, Scope),
	    M = {module,ID,Ps,Statement},
	    Ms = maps:get(module_list,Scope,[]),
	    {true,Scope#{ module_list=>[M|Ms]}};
	{function,_Line,{id,_,Name},Params,Expr} ->
	    Ps = eval_params(Params, Scope),
	    F = {function,Name,Ps,Expr},
	    Fs = maps:get(function_list,Scope,[]),
	    {true,Scope#{ function_list=>[F|Fs]}};
	{assign,_Line,{id,_,Var},Expr} ->
	    %% fixme: only eval Expr for the latest bindings in the scope
	    Expr1 = eval_expr(Expr, Scope),
	    {true, Scope#{Var=>Expr1}};
	{mcall,_Line,Tags,{id,_,ID},Args, Stmt1} ->
	    Obj = eval_mcall(ID, Args, Tags, Stmt1, Scope),
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

%% [!#%*] ID(Args) { Stmt1; Stmt2; ... }
eval_mcall(ID, Args, _Tags, Child, Scope) ->
    Ms = maps:get(module_list,Scope,[]),
    case lists:keyfind(ID, 2, Ms) of
	{module,_,_Ps,ModFun} when is_function(ModFun) ->
	    ModFun(Args, Child, Scope);
	{module,_,Ps,Stmt} ->
	    Values = eval_arg_list(Args, Scope),
	    Scope1 = bind(Ps, Values, Scope),
	    {Obj,_} = eval_stmt(Stmt, Scope1#{children=>Child}),
	    Obj
    end.

eval_for(Var, Range, Child, Scope) ->
    case Range of
	{range,{Start,Inc,End}} ->
	    Rs =
		fold_sequence(Start, End, Inc,
			      fun(E,Acc) ->
				      {Obj,_} = eval_stmt(Child,Scope#{Var=>E}),
				      [Obj|Acc]
			      end, []),
	    lists:reverse(Rs);
	Vec when is_list(Vec) ->
	    Rs =
		lists:foldl(fun(E,Acc) ->
				    {Obj,_} = eval_stmt(Child,Scope#{Var=>E}),
				    [Obj|Acc]
			    end, []),
	    lists:reverse(Rs)
    end.

eval_lc_for(Var, Range, Expr, Scope) ->
    case Range of
	{range,{Start,Inc,End}} ->
	    Rs =
		fold_sequence(Start, End, Inc,
			      fun(E,Acc) ->
				      [eval_expr(Expr, Scope#{Var=>E})|Acc]
			      end, []),
	    lists:reverse(Rs);
	Vec when is_list(Vec) ->
	    Rs =
		lists:foldl(fun(E,Acc) ->
				    [eval_expr(Expr, Scope#{Var=>E})|Acc]
			    end, []),
	    lists:reverse(Rs)
    end.

eval_child(empty, Op, _Scope) ->
    Op;
eval_child(Child, Color={color,_}, Scope) ->
    {Obj,_} = eval_stmt(Child, Scope#{color=>Color}),
    Obj;
eval_child(Child, {transform,T}, Scope) ->
    Ts = maps:get(transform, Scope, []),
    {Obj,_} = eval_stmt(Child, Scope#{transform=>[T|Ts]}),
    Obj;
eval_child(Child, {operation,Op}, Scope) ->
    {Obj,_} = eval_stmt(Child, Scope),
    {Op,Obj}.

default_function_list() ->
    [
     {function, "cos", ["x"], fun([X]) -> math:cos(deg2rad(X)) end},
     {function, "sin", ["x"], fun([X]) -> math:cos(deg2rad(X)) end},
     {function, "tan", ["x"], fun([X]) -> math:tan(deg2rad(X)) end},
     {function, "acos", ["x"], fun([X]) -> math:acos(deg2rad(X)) end},
     {function, "asin", ["x"], fun([X]) -> math:asin(deg2rad(X)) end},
     {function, "atan", ["x"], fun([X]) -> math:atan(deg2rad(X)) end},
     {function, "atan2", ["y","x"], fun([Y,X]) -> math:atan2(deg2rad(Y),deg2rad(X)) end},
     {function, "abs", ["x"], fun(X) -> abs(X) end}
     %% FIXME: add more functions (ALL)
    ].

default_module_list() ->
    [
     %% 3D objects
     {module, "cylinder", 
      ["h","r","r1","r2","center","d","d1","d2", "$fa","$fs","$fn"],
      fun mod_cylinder/3},
     {module, "sphere", 
      ["r","d", "$fa", "$fs", "$fn"], fun mod_sphere/3},
     {module, "cube", 
      ["size", "center"], fun mod_cube/3},
     %% Color
     {module, "color", ["c", "alpha"], fun mod_color/3},
     %% Transformations
     {module, "translate", ["v"], fun mod_translate/3},
     {module, "rotate", ["a","v"], fun mod_rotate/3},
     %% Operations
     {module, "for", ["*"], fun mod_for/3},
     {module, "echo", ["*"], fun mod_echo/3},
     {module, "intersection", ["*"], fun mod_intersection/3},
     {module, "union", ["*"], fun mod_union/3},
     {module, "difference", ["*"], fun mod_difference/3}
     %% ADD more operations/objects and transformations
    ].


default_scope() ->
    #{
      transform => [id],
      function_list => default_function_list(),
      module_list => default_module_list(),
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


eval_expr(empty, _Scope) ->
    true;
eval_expr({vector,_ln,Elems}, Scope) ->
    [eval_expr(E,Scope) || E <- Elems];
eval_expr({id,_Ln,Var}, Scope) ->
    %% should we override functions?
    case maps:get(Var, Scope, undef) of
	undef ->
	    Fs = maps:get(function_list, Scope, []),
	    case lists:keyfind(Var, 2, Fs) of
		{function,Var,Ps,Expr} ->
		    fun(Args) ->
			    Scope1 = bind(Ps, Args, Scope),
			    eval_expr(Expr, Scope1)
		    end;
		false ->
		    io:format("undefined variable ~p~n", [Var]),
		    undef
	    end;
	Val -> Val
    end;
eval_expr({number,_Ln,Num}, _Scope) -> Num;
eval_expr({string,_Ln,Str}, _Scope) -> Str;
eval_expr({true,_Ln}, _Scope) ->  true;
eval_expr({false,_Ln}, _Scope) -> false;
eval_expr({undef,_Ln}, _Scope) -> undef;
eval_expr({range,_Ln,Start,End}, Scope) ->
    S = eval_expr(Start, Scope),
    E = eval_expr(End, Scope),
    {range, {S, 1, E}};
eval_expr({range,_Ln,Start,Inc,End}, Scope) ->
    S = eval_expr(Start, Scope),
    I = eval_expr(Inc, Scope),
    E = eval_expr(End, Scope),
    {range, {S, I, E}};
eval_expr({lc_for,_Ln,Args,Expr}, Scope) ->
    case eval_arg_list(Args, Scope) of    
	[{Var,Range}|_] ->
	    eval_lc_for(Var, Range, Expr, Scope)
    end;
eval_expr({function,_Ln,Params,Expr}, Scope) ->
    Ps = eval_params(Params,Scope),
    fun(Vs) ->
	    Scope1 = bind(Ps, Vs, Scope),
	    eval_expr(Expr, Scope1)
    end;
eval_expr({'echo',_Ln,Args,Expr}, Scope) ->
    As = [eval_expr(A, Scope) || A <- Args],
    echo(As),
    eval_expr(Expr, Scope);
eval_expr({'assert',Ln,Args,Expr}, Scope) ->
    As = [eval_expr(A, Scope) || A <- Args],
    assert(As,Args,Ln,Scope),
    eval_expr(Expr, Scope);
eval_expr({op,_Ln,call,Call,Args},Scope) -> 
    Call1 = eval_expr(Call, Scope),
    if is_function(Call1) ->
	    Args1 = eval_arg_list(Args, Scope),
	    Call1(Args1);
       true ->
	    io:format("~p is not a function~n", [Call1]),
	    undef
    end;
eval_expr({op,_Ln,Op,Arg1},Scope) -> 
    A1 = eval_expr(Arg1, Scope),
    case Op of
	'+' -> A1;
	'-' -> -A1;
	'!' -> not A1;
	_ -> undef
    end;
eval_expr({op,_Ln,Op,Arg1,Arg2},Scope) ->
    A1 = eval_expr(Arg1, Scope),
    A2 = eval_expr(Arg2, Scope),
    case Op of
	'+' -> A1 + A2;   %% fixme: vector addition
	'-' -> A1 - A2;   %% fixme: vector subtraction
	'*' -> A1 * A2;
	'/' -> A1 / A2;
	'%' -> A1 rem A2;
	'==' -> A1 == A2;
	'!=' -> A1 /= A2;
	'>' -> A1 > A2;
	'>=' -> A1 >= A2;
	'<' -> A1 < A2;
	'<=' -> A1 =< A2;
	'&&' -> bool(A1) andalso bool(A2);
	'||' -> bool(A1) orelse bool(A2);
	'^' -> math:pow(A1,A2);
	_ -> undef
    end;
eval_expr({op,_Ln,'?',Cond,Then,Else},Scope) ->
    C = eval_expr(Cond,Scope),
    case bool(C) of
	true -> eval_expr(Then, Scope);
	false -> eval_expr(Else, Scope)
    end.

eval_arg_list([{'=',{id,_Ln,ID},Arg}|Args], Scope) ->
    Val = eval_expr(Arg, Scope),
    [{ID,Val}|eval_arg_list(Args, Scope)];
eval_arg_list([Arg|Args], Scope) ->
    Val = eval_expr(Arg, Scope),
    [Val|eval_arg_list(Args, Scope)];
eval_arg_list([], _Scope) ->
    [].


eval_arg_list([{'=',{id,_Ln,ID},Expr}|Args],[_|Order],Scope) ->
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

eval_params([{'=',{id,_Ln,ID},Expr}|Args], Scope) ->
    Val = eval_expr(Expr, Scope),
    [{ID,Val}|eval_params(Args, Scope)];
eval_params([{id,_Ln,ID}|Args], Scope) ->
    [ID | eval_params(Args, Scope)];
eval_params([], _Scope) ->
    [].

%% fixme: understand the binding rules 
bind([Name|Ps], [V|Vs], Scope) when is_list(Name) ->
    bind(Ps, Vs, Scope#{ Name => V });
bind([{Name,Default}|Ps], Vs, Scope) ->
    bind(Ps, Vs, Scope#{ Name => Default });
bind([], _, Scope) ->
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

mod_union(Args, Child, Scope) ->
    Order = [],
    _Prop = eval_arg_list(Args, Order, Scope),
    Op = {operation, union},
    eval_child(Child, Op, Scope).

mod_difference(Args, Child, Scope) ->
    Order = [],
    _Prop = eval_arg_list(Args, Order, Scope),
    Op = {operation, difference},
    eval_child(Child, Op, Scope).

mod_intersection(Args, Child, Scope) ->
    Order = [],
    _Prop = eval_arg_list(Args, Order, Scope),
    Op = {operation, intersection},
    eval_child(Child, Op, Scope).

mod_echo(Args, _Child, Scope) ->
    As = eval_arg_list(Args, Scope),
    echo(As),
    true.

mod_for(Args, Child, Scope) ->
    case eval_arg_list(Args, Scope) of
	[{Var,Range}|_] ->
	    eval_for(Var, Range, Child, Scope)
    end.

mod_translate(Args, Child, Scope) ->
    Order = ["v"],
    Props = eval_arg_list(Args, Order, Scope),
    V = proplists:get_value("v", Props, undef),
    T = {transform,{translate,[{v,V}]}},
    eval_child(Child, T, Scope).

mod_rotate(Args, Child, Scope) ->
    Order = ["a", "v"],
    Props = eval_arg_list(Args, Order, Scope),
    A = proplists:get_value("a", Props, 0),
    V = proplists:get_value("v", Props, undef),
    T = {transform,{rotate,[{a,A},{v,V}]}},
    eval_child(Child, T, Scope).

mod_color(Args, Child, Scope) ->
    Order = ["c", "alpha"],
    Props = eval_arg_list(Args, Order, Scope),
    C = proplists:get_value("c", Props, undef),
    A = proplists:get_value("alpha", Props, undef),
    Color = {color,[{c,C},{alpha,A}]},
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
    {cylinder,[{h,H},{r1,R1},{r2,R2},{center,Center}],
     maps:get(transform, Scope, []), maps:get(color, Scope, [])}.
    
mod_sphere(Args, _Child, Scope) ->    
    Order = ["r","d", "$fa", "$fs", "$fn"],
    Res = eval_arg_list(Args, Order, Scope),
    R = case proplists:get_value("d", Res, undef) of
	    undef -> proplists:get_value("r", Res, 1);
	    D -> D/2
	end,
    {sphere,[{r,R}],
     maps:get(transform, Scope, []), maps:get(color, Scope, [])}.

mod_cube(Args, _Child, Scope) ->
    Order = ["size","center"],
    Props = eval_arg_list(Args, Order, Scope),
    Size = proplists:get_value("size", Props, [1,1,1]),
    Center = proplists:get_value("center", Props, false),
    {cube,[{size,Size},{center,Center}],
     maps:get(transform, Scope, []), maps:get(color, Scope, [])}.
