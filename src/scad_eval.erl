%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%	OpenScad runtime functions
%%% @end
%%% Created : 24 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(scad_eval).

-export([file/1, file/2]).
-export([string/1, string/2]).
-export([stmt_list/1, stmt/2, expr/2]).
-export([color/1]).
-export([default_scope/0]).

-export([rad2deg/1, deg2rad/1, bool/1,
	 lt/2, lte/2, gt/2, gte/2, eq/2, neq/2,
	 add/2, subtract/2, negate/1,
	 multiply/2, divide/2, scale/2,
	 reminder/2, dot_/2, norm/1, cross/2,
	 lookup/2, matmul_/2, transpose/1]).

-export([mod_union/3, mod_difference/3, mod_intersection/3]).
-export([mod_echo/3, mod_children/3]).
-export([mod_translate/3, mod_rotate/3, mod_scale/3]).
-export([mod_color/3]).
-export([mod_cylinder/3, mod_sphere/3, mod_cube/3]).
	 
-export([cos/1, sin/1, tan/1, acos/1, asin/1, atan/1, atan2/1]).


-include("scad.hrl").


%% -define(OUTPUT_PREFIX, "// ").
-define(OUTPUT_PREFIX, "").

%% -define(dbg(F,A), io:format(F,A)).
-define(dbg(F,A), ok).


-type vector() :: [number()|vector()] | [].

file(Filename) ->
    file(Filename, []).
file(Filename, Opts) ->
    {ok, Stmts} = scad_lint:file(Filename, Opts),
    stmt_list(Stmts).

string(String) ->
    string(String, []).
string(String, Opts) ->
    {ok, Stmts} = scad_lint:string(String, Opts),
    stmt_list(Stmts).

stmt_list(Stmts) ->
    Scope = default_scope(),
    {Objs,_} = stmt_list_(Stmts, Scope),
    lists:reverse(Objs).

%% returns list of objects reverses!
stmt_list_(Stmts, Scope) ->
    stmt_list_(Stmts, Scope, []).

stmt_list_([{file,Filename}|Stmts], Scope, Acc) ->
    stmt_list_(Stmts, Scope#{filename=>Filename}, Acc);
stmt_list_([Stmt|Stmts], Scope, Acc) ->
    {Obj,Scope1} = stmt(Stmt, Scope),
    stmt_list_(Stmts, Scope1, prepend(Obj, Acc));
stmt_list_([], Scope, Acc) ->
    {Acc, Scope}.

prepend([Obj|Objs], Acc) -> prepend(Objs,[Obj|Acc]);
prepend([], Acc) -> Acc;
prepend(true, Acc) -> Acc;
prepend(Obj, Acc) -> [Obj|Acc].

append(Objs, Acc) when is_list(Objs) -> Acc ++ Objs;
append(true, Acc) -> Acc;
append(Obj, Acc) -> Acc ++ [Obj].
     

stmt(Stmt, Scope) ->
    case Stmt of
	empty ->
	    {true, Scope};  %% empty?
	{block,Stmts} -> %% not a scope
	    stmt_list_(Stmts, Scope);
	#mod{name=Name,use=Bind} ->
	    Bound = get_vars(Bind, Scope),
	    Mod = Stmt#mod{use=Bound},
	    Scope1 = set_module(Name, Mod, Scope),
	    {true,Scope1};
	#func{name=Name,use=Bind} ->
	    Bound = get_vars(Bind, Scope),
	    Func = Stmt#func{use=Bound},
	    Scope1 = set_function(Name, Func, Scope),
	    {true,Scope1};
	{'=',Var,Expr} ->
	    Value = expr(Expr, Scope),
	    {true, set(Var, Value, Scope)};
	{'let',_Tags,Args,ChildStmt} ->
	    Scope1 = let_args(Args, Scope),
	    stmt(ChildStmt, Scope1);
	{'for',_Tags,Args,ChildStmt} ->
	    for(Args, ChildStmt, Scope);
	{mcall,ID,Tags,Args,ChildStmt} ->
	    Obj = mcall(ID, Args, Tags, ChildStmt, Scope),
	    {Obj, Scope};
	{'if', Cond, Then, Else} ->
	    C = expr(Cond, Scope),
	    case bool(C) of
		true -> stmt(Then, Scope);
		false -> stmt(Else, Scope)
	    end;
	{'if', Cond, Then} ->
	    C = expr(Cond, Scope),
	    case bool(C) of
		true -> stmt(Then, Scope);
		false -> {true,Scope}
	    end
    end.

%% [!#%*] ID(Args) { Stmt1; Stmt2; ... }
mcall(ID, Args, _Tags, ChildStmt, Scope0) ->
    case get_module(ID, Scope0) of
	#mod{params=_Params,stmt=ModFun} when is_function(ModFun) ->
	    Args1 = args(Args, Scope0),
	    ModFun(Args1, ChildStmt, Scope0);
	#mod{use=Bound,params=Params,stmt=Stmt} ->
	    Args1 = args(Args, Scope0),
	    Scope1 = set(Bound, Scope0),  %% update scope with bound variables
	    %% {Values0,PMap} = params(Params, Scope1),
	    ?dbg("eval: mcall ~s,bound=~p,params=~p\n",
		      [ID,Bound,Params]),
	    ?dbg("eval: mcall args = ~p\n", [Args1]),
	    %% Scope2 = bindx(Args1, Values0, PMap, Scope1),
	    Scope2 = bind0(Params, Args1, Scope1),
	    Children = child(ChildStmt, Scope0),
	    Len = length(Children),
	    Parents = maps:get(parent_modules,Scope0,[]),
	    Parents1 = [ID|Parents],
	    {Obj,_} = stmt(Stmt, Scope2#{"$children" => Len,
					 children=>Children,
					 parent_modules=>Parents1,
					 "$parent_modules"=>Parents1}),
	    Obj
    end.

%% for (v1 = range1, ...  vn = rangen) { stmts }
for([{'=',Var,Range}|Args], Stmts, Scope) ->
    case expr(Range, Scope) of
	_R = {range,{S,I,E}} ->
	    fold_sequence(S, E, I,
			  fun(Val, {Acc,Sa}) ->
				  Sb = set(Var, Val, Sa),
				  {Obj,Sc} = for(Args, Stmts, Sb),
				  {append(Obj,Acc),Sc}
			  end, {[],Scope});
	Vec when is_list(Vec) ->
	    lists:foldl(fun(Val, {Sa,Acc}) ->
				Sb = set(Var, Val, Sa),
				{Obj,Sc} = for(Args, Stmts, Sb),
				{append(Obj,Acc),Sc}
			end, {[],Scope}, Vec)
    end;
for([], Smts, Scope) ->
    stmt(Smts, Scope).

expr(Expr, Scope) ->
    try expr_(Expr, Scope) of
	Value -> Value
    catch
	error:_:_Stack ->
	    ?dbg("eval_expr: failed ~p\~p\n", [Expr,_Stack]),
	    undef
    end.

expr_({vector,Elems}, Scope) ->
    vector(Elems, Scope);
expr_({var,Var}, Scope) ->
     maps:get(Var, Scope, undef);
expr_({range,{Start,Inc,End}}, Scope) ->
    S = expr_(Start, Scope),
    I = expr_(Inc, Scope),
    E = expr_(End, Scope),
    {range, {S, I, E}};
expr_(#func{use=Bind,params=Params,expr=Expr}, Scope) ->
    Bound = get_vars(Bind, Scope),
    %% FIXME handle named parameters
    fun(Vs,FScope) ->
	    FScope1 = set(Bound, FScope),  %% update scope with bound variables
	    FScope2 = bind(Params, Vs, FScope1),
	    expr(Expr, FScope2)
    end;
expr_({'echo',Args,Expr}, Scope) ->
    As = [expr_(A, Scope) || A <- Args],
    echo(As),
    expr_(Expr, Scope);
expr_({'assert',Line,Args,Expr}, Scope) ->
    As = [expr(A, Scope) || A <- Args],
    assert(As,Args,Line,Scope),
    expr(Expr, Scope);
expr_({'let',Args,Expr}, Scope) ->
    Scope1 = let_args(Args, Scope),
    expr_(Expr, Scope1);
expr_({op,call,Call,Args},Scope) ->
    case call(Call, Scope) of
	{Params,Map,Fun} when is_function(Fun) ->
	    Args1 = args(Args, Scope),
	    Args2 = args_to_vec(Args1, Map, params_to_tuple(Params, Args1)),
	    Fun(Args2,Scope);
	true ->
	    ?dbg("~p is not a function~n", [Call1]),
	    undef
    end;
expr_({op,Op,Arg1},Scope) -> 
    A1 = expr_(Arg1, Scope),
    case Op of
	'+' -> A1;
	'-' -> negate(A1);
	'!' -> not bool(A1);
	_ -> undef
    end;
expr_({op,Op,Arg1,Arg2},Scope) ->
    A1 = expr_(Arg1, Scope),
    A2 = expr_(Arg2, Scope),
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
	index -> index(A2, A1);
	_ -> undef
    end;
expr_({op,'?',Cond,Then,Else},Scope) ->
    C = expr_(Cond,Scope),
    case bool(C) of
	true -> expr_(Then, Scope);
	false -> expr_(Else, Scope)
    end;
%% number/string/boolan/undef/empty
expr_(Constant, _Scope) -> Constant.


call(#func{use=Bind,params=Params,map=Map,expr=Expr}, Scope) ->
    Bound = get_vars(Bind, Scope),
    {Params,Map,
     fun(Args,FScope) ->
	     FScope1 = set(Bound, FScope),
	     if is_function(Expr) ->
		     Expr(Args, FScope1);
		true ->
		     FScope2 = bind0(Params, Args, FScope1),
		     expr(Expr, FScope2)
	     end
     end};
call({var,Var}, Scope) ->
    case maps:get(Var, Scope, undef) of
	undef -> %% not anonomous function
	    undef;
	#func{name=anonymous,use=Bind,params=Params,map=Map,expr=Expr} ->
	    Bound = get_vars(Bind, Scope),
	    {Params,Map,
	     fun(Args,FScope) ->
		     FScope1 = set(Bound, FScope),
		     FScope2 = bind0(Params, Args, FScope1),
		     expr(Expr, FScope2)
	     end};
	_ ->
	    io:format("not a function ~p~n", [Var]),
	    undef
    end;
call(Expr, Scope) ->
    case expr_(Expr, Scope) of
	Func={_Parms,_Map,Fun} when is_function(Fun) ->
	    Func;
	_Value ->
	    io:format("not a function ~p~n", [_Value]),
	    undef
    end.


vector([Element|Es], Scope) ->
    vector_element(Element, Scope) ++ vector(Es, Scope);
vector([], _Scope) ->
    [].

vector_element(Element, Scope) ->
    case Element of
	{lc_for,Args,Element1} ->
	    case args(Args, Scope) of %% Fixme: multiple Args...
		[{Var,Range}|_] ->
		    lc_for(Var, Range, Element1, Scope)
	    end;

	{lc_let,Args,Element1} ->
	    Scope1 = let_args(Args, Scope),
	    vector_element(Element1, Scope1);

	{lc_each, Element1} ->
	    vector_element(Element1, Scope);

	{lc_forc,Init,Cond,Update,Element1} ->
	    Scope1 = let_args(Init, Scope),
	    lc_forc(Cond, Update, Element1, Scope1);

	{lc_if,Cond,Then} ->
	    case bool(expr(Cond, Scope)) of
		true -> vector_element(Then, Scope);
		false -> []
	    end;
	    
	{lc_if,Cond,Then,Else} ->
	    case bool(expr(Cond, Scope)) of
		true -> vector_element(Then, Scope);
		false -> vector_element(Else, Scope)
	    end;
	_ ->
	    [expr(Element, Scope)]
    end.


lc_forc(Cond, Update, Element, Scope) ->
    case bool(expr(Cond, Scope)) of
	true ->
	    Value = vector_element(Element, Scope),
	    Scope1 = let_args(Update, Scope),
	    [Value | lc_forc(Cond, Update, Element, Scope1)];
	false ->
	    []
    end.

lc_for(Var, Range, Expr, Scope) ->
    case Range of
	{range,{Start,Inc,End}} ->
	    Rs =
		fold_sequence(Start, End, Inc,
			      fun(Val,Acc) ->
				      Scope1 = set(Var,Val, Scope),
				      [expr(Expr, Scope1)|Acc]
			      end, []),
	    lists:reverse(Rs);
	Vec when is_list(Vec) ->
	    Rs =
		lists:foldl(fun(Val,Acc) ->
				    Scope1 = set(Var,Val, Scope),
				    [expr(Expr, Scope1)|Acc]
			    end, []),
	    lists:reverse(Rs)
    end.



%% sequential bind of
let_args([{'=',Var,Arg}|Args], Scope) ->
    Val = expr(Arg, Scope),
    let_args(Args, set(Var,Val,Scope));
let_args([], Scope) ->
    Scope.

args([{'=',Var,Arg}|Args], Scope) ->
    Val = expr(Arg, Scope),
    [{Var,Val}|args(Args, Scope)];
args([Arg|Args], Scope) ->
    Val = expr(Arg, Scope),
    [Val|args(Args, Scope)];
args([], _Scope) ->
    [].

%% params(Params, Scope) ->
%%     params(Params, Scope, 1, [], #{}).
%% params([{'=',Var,Expr}|Args], Scope, I, As, Map) ->
%%     Val = expr(Expr, Scope),
%%     params(Args, Scope, I+1, [Val|As], Map#{Var=>I});
%% params([Var|Args], Scope, I, As, Map) ->
%%     params(Args, Scope, I+1, [undef|As], Map#{Var=>I});
%% params([], _Scope, _I, As, Map) ->
%%     {lists:reverse(As), Map}.

%% return a vector of default paramters
params_to_tuple(?vararg, Args) ->
    erlang:make_tuple(length(Args), undef);
params_to_tuple(Params, _Args) ->
    params_to_tuple_(Params, []).

params_to_tuple_([{'=',Var,Default}|Params], Acc) when ?is_var(Var) ->
    params_to_tuple_(Params, [Default|Acc]);
params_to_tuple_([Var|Params], Acc) when ?is_var(Var) ->
    params_to_tuple_(Params, [undef|Acc]);
params_to_tuple_([], Acc) ->
    list_to_tuple(lists:reverse(Acc)).

args_to_vec(Args, Map, Tuple) ->
    args_to_vec(Args, 1, Map, Tuple).
args_to_vec([{'=',Var,Expr}|Args], I, Map, Tuple) when ?is_var(Var) ->
    J = maps:get(Var, Map),
    args_to_vec(Args, I, Map, setelement(J, Tuple, Expr));
args_to_vec([Expr|Args], I, Map, Tuple) ->
    args_to_vec(Args, I+1, Map, setelement(I, Tuple, Expr));
args_to_vec([], _I, _Map, Tuple) ->
    tuple_to_list(Tuple).


%% Extract value from variables in Vs
get_vars(Vs, Scope) when is_list(Vs) ->
    [{V,maps:get(V, Scope, undef)} || V <- Vs];
get_vars(Vs, Scope) -> %% assume set!
    get_vars(sets:to_list(Vs), Scope).

%% bindx(Args, Values, PMap, Scope) ->
%%     bindx(Args, Values, Values, PMap, Scope).
%% bindx([{'=',Var,Value} | Args], Values, Values0, PMap, Scope) ->
%%     I = maps:get(Var, PMap, undef),
%%     case I of
%% 	undef ->
%% 	    bindx(Args, Values, Values0, PMap, set(Var,Value,Scope));
%% 	_ ->
%% 	    bindx(Args, Values, Values0, PMap, set(Var,lists:nth(I,Values0),
%% 						   Scope))
%%     end;
%% bindx([Var | Args], [Value|Values], Values0, PMap, Scope) ->
%%     bindx(Args, Values, Values0, PMap, set(Var,Value,Scope));
%% bindx([], _Values, _Values0, _PMap, Scope) ->
%% 	Scope.

%% fixme: understand the binding rules 
bind([{'=',Var,Default}|Ps], Vs, Scope) ->
    Value = proplists:get_value(Var, Vs, Default),
    bind(Ps, Vs, set(Var,Value,Scope));
bind([Var|Ps], [Val|Vs], Scope) when is_list(Var) ->
    bind(Ps, Vs, set(Var,Val,Scope));
bind([], _, Scope) ->
    Scope.

%% fixme: understand the binding rules 
bind0([{'=',Var,_Default}|Ps], [Val|As], Scope) ->
    bind0(Ps, As, set(Var,Val,Scope));
bind0([Var|Ps], [Val|Vs], Scope) when is_list(Var) ->
    bind0(Ps, Vs, set(Var,Val,Scope));
bind0([], _, Scope) ->
    Scope.


%% set a variable to a value in a scope
set(Name, Value, Scope) ->
    Scope#{ Name => Value }.

%% bind a list of variables to values in a scope
set([{Name,Value}|Vs], Scope) ->
    set(Vs, set(Name,Value,Scope));
set([], Scope) ->
    Scope.

echo(As) ->
    Data =
	lists:join(",",
		   [case A of
			{ID,Value} ->
			    [ID," = ", format_value(Value)];
			Value ->
			    format_value(Value)
		    end || A <- As]),
    io:format("~sECHO: ~s\n", [?OUTPUT_PREFIX,Data]).

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
	    io:format("~sERROR: Assertion '~p' failed in file ~s, line ~p\n", 
		      [?OUTPUT_PREFIX,Expr,Filename,Line]),
	    ?dbg("scope: ~p\n", [Scope]),
	    throw(assertion_failed);
       true ->
	    true
    end;
assert([Cond,Message],[Expr|_],Line,Scope) ->
    if not Cond ->
	    Filename = maps:get(filename, Scope, "unknown"),
	    io:format("~sERROR: Assertion '~p' ~p, failed in file ~s, line ~p\n", 
		      [?OUTPUT_PREFIX,Expr,Message,Filename,Line]),
	    ?dbg("scope: ~p\n", [Scope]),
	    throw(assertion_failed);
       true ->
	    true
    end.    

%% FIXME: use this
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

map_sequence(Start, Stop, Step, Fun) ->
    lists:reverse(fold_sequence(Start, Stop, Step, fun(X,Acc) -> [Fun(X)|Acc] end, [])).

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


-ifdef(not_used).
arg_list([{Var,Expr}|Args],[_|Order],Scope) ->
    Val = expr(Expr, Scope),
    [{Var,Val}|arg_list(Args, Order, Scope)];
arg_list([Arg|Args], [Var|Order], Scope) ->
    Val = expr(Arg, Scope),
    [{Var,Val}|arg_list(Args, Order, Scope)];
arg_list([], [Var=[$$|_]|Order], Scope) -> 
    case maps:get(Var, Scope, undef) of
	undef -> arg_list([], Order, Scope);
	Val -> [{Var,Val}|arg_list([], Order, Scope)]
    end;
arg_list([], [_Var|Order], Scope) ->
    arg_list([], Order, Scope);
arg_list([], [], _Scope) -> 
    [];
arg_list(Rest, [], _Scope) -> 
    [{rest,Rest}].
-endif.

index([Ind], Vec) when is_integer(Ind), is_list(Vec) ->
    lists:nth(Ind+1, Vec).

-spec rad2deg(number()) -> number().
rad2deg(Rad) ->
    Rad * 180 / math:pi().

-spec deg2rad(number()) -> number().
deg2rad(Deg) ->
    Deg * math:pi() / 180.	

-spec bool(any()) -> boolean().
bool(false) -> false;
bool(Num) when is_number(Num) -> not (Num == 0);
bool(<<>>) -> false;  %% empty string
bool([]) -> false;    %% empty vec
bool(undef) -> false;
bool(X) when not is_tuple(X) -> true.

eq(A, B) -> A == B.
neq(A, B) -> A =/= B.
gt(A, B) -> A > B.
gte(A, B) -> A >= B.
lt(A, B) -> A < B.
lte(A, B) -> A =< B.

-spec add(number()|vector(), number()|vector()) -> number()|vector().
add(A,B) when is_number(A), is_number(B) -> A + B;
add([A|As],[B|Bs]) -> [add(A,B)|add(As,Bs)];
add([], _) -> [];
add(_, []) -> [].

-spec subtract(number()|vector(), number()|vector()) -> number()|vector().
subtract(A,B) when is_number(A), is_number(B) -> A - B;
subtract([A|As],[B|Bs]) -> [subtract(A,B)|subtract(As,Bs)];
subtract([], _) -> [];
subtract(_, []) -> [].

-spec negate(number()|vector()) -> number()|vector().
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

%%matmul(As,Bs) when is_list(As), is_list(hd(As)),
%%		   is_list(Bs), is_list(hd(Bs)) ->
%%    matmul_(As, transpose(Bs)).

matmul_([A|As], Bs) when is_list(A) ->
    [ [dot_(A, B) || B <- Bs] | matmul_(As, Bs)];
matmul_([], _) -> [].

transpose([A|As]) when is_list(A) ->
    [ [lists:nth(N, V) || V <- [A|As]] || N <- lists:seq(1,length(A))].


get_function(Name, Bound) ->
    Funcs = maps:get(functions, Bound),
    maps:get(Name, Funcs, undefined).
set_function(Name, Func, Bound=#{functions:=Funcs}) ->
    Bound#{functions => maps:put(Name, Func, Funcs)}.
    
get_module(Name, Bound) ->
    Mods = maps:get(modules, Bound),
    maps:get(Name, Mods, undefined).
set_module(Name, Mod, Bound=#{modules:=Mods}) ->
    Bound#{modules => maps:put(Name, Mod, Mods)}.


default_scope() ->
    #{
      functions => builtin_functions(),
      modules   => builtin_modules(),
      filename => "unknown",
      "PI" => math:pi(),  %% check that this is constant!
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


-define(BIFx(Name,Fun),
	Name => #func{name=Name,params=["x"],map=#{"x"=>1},expr=Fun}).
-define(BIF(Name,Params,Fun),
	Name => #func{name=Name,params=Params,expr=Fun}).
-define(BIF(Name,Params,Map,Fun),
	Name => #func{name=Name,params=Params,map=Map,expr=Fun}).

builtin_functions() ->
    #{
      "version" => #func{name="version", expr=[2024,1,24]},
      "version_num" => #func{name="version_num",expr=20240124},
      ?BIFx("cos", fun scad_eval:cos/1),
      ?BIFx("sin", fun scad_eval:sin/1),
      ?BIFx("tan", fun scad_eval:tan/1),
      ?BIFx("acos", fun scad_eval:acos/1),
      ?BIFx("asin", fun scad_eval:acsin/1),
      ?BIFx("atan", fun scad_eval:atan/1),
      ?BIF("atan2", ["y","x"], #{ "y"=>1, "x"=>2}, fun scal_eval:atan/2),
      ?BIFx("abs", fun([X]) -> abs(X) end),
      ?BIFx("ceil", fun([X]) -> trunc(math:ceil(X)) end),
      ?BIF("concat",?vararg, fun(X) -> lists:flatten(X) end),
      ?BIF("cross",["a","b"],#{"a"=>1,"b"=>2},
	   fun([A,B]) -> scad_eval:cross(A,B) end),
      ?BIFx("exp", fun([X]) -> math:exp(X) end),
      ?BIFx("floor", fun([X]) -> trunc(math:floor(X)) end),
      ?BIFx("ln", fun([X]) -> math:log(X) end),
      ?BIFx("len", fun([X]) when is_binary(X) -> byte_size(X); %%fixme
		      ([X]) when is_list(X) -> length(X)
		   end),
      ?BIFx("log", fun([X]) -> math:log10(X) end),
      ?BIF("lookup",["k","kv"], #{"k"=>1, "kv"=>2},
	   fun([K,KV]) -> scad_eval:lookup(K,KV) end),
      ?BIF("max",?vararg, fun([V]) when is_list(V) -> lists:max(V);
			     ([X,Y]) when is_number(X), is_number(Y) -> max(X,Y);
			     (Xs) -> lists:max(Xs)
			  end),
      ?BIF("min",?vararg, fun([V]) when is_list(V) -> lists:min(V);
			     ([X,Y]) when is_number(X), is_number(Y) -> min(X,Y);
			     (Xs) -> lists:min(Xs)
			  end),
      ?BIF("norm",["v"],#{ "v"=>1},fun([V]) -> scad_eval:norm(V) end),
      ?BIF("pow",["x","y"],#{ "x"=>1, "y"=>2},
	   fun([X,Y]) -> math:pow(X,Y) end),
      ?BIF("rands",["min_value","max_value",{"value_count",1},
		    {"seed_value",undef}],
	   #{"min_value"=>1,"max_value"=>2, "value_count"=>3, "seed_value"=>4},
	   fun([A,B,N,undef]) when is_number(A), is_number(B), A =< B,
				   is_integer(N), N >= 0 ->
		   D = B-A,
		   [A+D*rand:uniform() || _ <- lists:seq(1,N)];
	      ([A,B,N,Seed]) when is_number(A), is_number(B), A =< B,
				  is_integer(N), N >= 0, 
				  is_integer(Seed) ->
		   rand:seed(exsss,Seed),
		   D = B-A,
		   [A+D*rand:uniform() || _ <- lists:seq(1,N)]
	   end),
      ?BIFx("round", fun([X]) -> round(X) end),
      ?BIFx("sign", fun([X]) when X > 0 -> 1;
			    ([X]) when X < 0 -> -1;
			    ([_]) -> 0
			 end),
      ?BIFx("sqrt", fun([X]) -> math:sqrt(X) end),
      ?BIF("parent_module",["n"], #{ "n"=>1 },
	   fun([N],Scope) ->
		   Ps = maps:get(parent_modules,Scope,[]),
		   lists:nth(N+1, Ps)
	   end),
      ?BIFx("str", fun([X]) -> io_lib:format("~p", [X]) end),
      ?BIFx("chr", fun([X]) -> [X] end),
      ?BIFx("ord", fun([X]) -> hd(X) end),
      ?BIFx("is_string", fun([X]) -> is_binary(X) end),
      ?BIFx("is_list", fun([X]) -> is_list(X) end),
      ?BIFx("is_num",  fun([X]) -> is_number(X) end),
      ?BIFx("is_bool", fun([X]) -> is_boolean(X) end),
      ?BIFx("is_undef", fun([X]) -> X == undef end),
      ?BIFx("is_function", fun([X]) -> is_function(X) end)
     }.

cos([X]) -> math:cos(deg2rad(X)).
sin([X]) -> math:sin(deg2rad(X)).
tan([X]) -> math:tan(deg2rad(X)).
acos([X]) -> rad2deg(math:acos(X)).
asin([X]) -> rad2deg(math:asin(X)).
atan([X]) -> rad2deg(math:atan(X)).
atan2([Y,X]) -> rad2deg(math:atan2(Y,X)).

norm([]) -> 0;
norm(As) when is_list(As), is_number(hd(As))  ->
    math:sqrt(lists:sum([A*A || A <- As])).


-define(BIM(Name,Params,Fun),
	Name => #mod{name=Name,params=Params,stmt=Fun}).
-define(BIM(Name,Params,Map,Fun),
	Name => #mod{name=Name,params=Params,map=Map,stmt=Fun}).

builtin_modules() ->
    #{
      %% "search" => ...
      %% FIXME: add more functions (ALL)
      %% 3D objects
      ?BIM("cylinder",
	   ["h","r","r1","r2","center","d","d1","d2"],
	   %% "$fa","$fs","$fn"
	   #{"h"=>1,"r"=>2,"r1"=>3,"r2"=>4,"center"=>5,"d"=>6,"d1"=>7,"d2"=>8},
	   fun scad_eval:mod_cylinder/3),
      ?BIM("sphere",["r","d"],
	   %% "$fa", "$fs", "$fn"],
	   #{"r"=>1,"d"=>2},
	   fun scad_eval:mod_sphere/3),
      ?BIM("cube",["size", "center"],
	   #{"size"=>1, "center"=>2},
	   fun scad_eval:mod_cube/3),
      %% Color
      ?BIM("color",["c", "alpha"], fun scad_eval:mod_color/3),
      %% Transformations
      ?BIM("translate",["v"], #{ "v" => 1 }, fun scad_eval:mod_translate/3),
      ?BIM("rotate",["a","v"], #{ "a" => 1, "v" => 2 }, fun scad_eval:mod_rotate/3),
      ?BIM("scale",["v"], #{ "v" => 1 }, fun scad_eval:mod_scale/3),
      %% Operations
      ?BIM("assert",?vararg, fun scad_eval:mod_assert/3),
      ?BIM("echo",?vararg, fun scad_eval:mod_echo/3),

      ?BIM("children", ?vararg, fun scad_eval:mod_children/3),
      ?BIM("intersection",[], fun scad_eval:mod_intersection/3),
      ?BIM("union",[], fun scad_eval:mod_union/3),
      ?BIM("difference",[], fun scad_eval:mod_difference/3)
      %% ADD more operations/objects and transformations
     }.

%%
%% built in modules - move to various files?
%%

mod_union([], Child, Scope) ->
    Children = child(Child, Scope),
    #object{type=union, children=Children}.
    

mod_difference([], Child, Scope) ->
    Children = child(Child, Scope),
    #object {type=difference, children=Children}.

mod_intersection([], Child, Scope) ->
    Children = child(Child, Scope),
    #object {type=intersection, children=Children}.

mod_echo(Args, _Child, Scope) ->
    As = args(Args, Scope),
    ?dbg("echo: args = ~p\n", [As]),
    echo(As),
    true.

mod_children([], _Child, Scope) ->
    maps:get(children, Scope);  %% flatten? filter true?
mod_children([A], _Child, Scope) ->
    Cs = maps:get(children, Scope),
    case expr(A, Scope) of
	I when is_integer(I) ->
	    lists:nth(I+1, Cs);
	{range,{S,I,E}} ->
	    List = sequence(S, E, I),
	    lists:map(
	      fun(J) ->
		      lists:nth(J+1,Cs)
	      end, List)
    end.

mod_translate([V], Child, Scope) ->
    Children = child(Child, Scope),
    #object { type=translate, params=[{v,V}], children=Children}.

mod_scale([V], Child, Scope) ->
    Children = child(Child, Scope),
    #object { type=scale, params=[{v,V}],children=Children}.

mod_rotate([A,V], Child, Scope) ->
    AA = if A =:= undef -> 0; true -> A end,
    Children = child(Child, Scope),
    #object{type=rotate,params=[{a,AA},{v,V}],children=Children}.

mod_color([C,Alpha], Child, Scope) ->
    Children = child(Child, Scope),
    #object {type=color,params=[{c,C},{alpha,Alpha}],children=Children}.

mod_cylinder([H,R,R1,R2,Center,D,D1,D2|_], _Child, _Scope) ->
    %% "$fa","$fs","$fn"],
    HH = if H =:= undef -> 1; true -> H end,
    RR1 = if D1 == undef, D /= undef -> D/2;
	     D1 == undef -> if R1 == undef -> R; true -> R1 end;
	     true -> D1/2
	  end,
    RR2 = if D2 == undef, D /= undef -> D/2;
	     D2 == undef -> if R2 == undef -> R; true -> R2 end;
	     true -> D2/2
	  end,
    Center1 = if Center =:= undef -> false; true -> Center end,
    #object{type=cylinder,
	    params=[{h,HH},{r1,RR1},{r2,RR2},{center,Center1}]}.

mod_sphere([R,D|_], _Child, _Scope) ->
    %% "$fa", "$fs", "$fn"],
    R1 = if D == undef ->
		 if R == undef -> 1; true -> R end;
	    true ->
		D/2
	 end,
    #object{type=sphere,params=[{r,R1}]}.

mod_cube([Size,Center], _Child, _Scope) ->
    Size1 = if Size =:= undef -> [1,1,1]; true -> Size end,
    Center1 = if Center =:= undef -> false; true -> Center end,
    #object{type=cube, params=[{size,Size1},{center,Center1}]}.


child(empty, _Scope) -> [];
child({block,Stmts}, Scope) -> %% scope!
    {Objs,_} = stmt_list_(Stmts, Scope),
    lists:reverse(Objs);
child(Stmt, Scope) -> 
    case stmt(Stmt, Scope) of
	{Objs,_} when is_list(Objs) -> Objs;
	{true,_} -> [];
	{Obj,_} -> [Obj]
    end.
