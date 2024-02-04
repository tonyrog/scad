%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Lint and precompile scad files
%%% @end
%%% Created : 24 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(scad_lint).
-export([file/1, file/2]).
-export([string/1, string/2]).
-export([lint/1, lint/2]).

-include("./scad.hrl").

-define(is_value(X), (not is_tuple((X)))).

-define(dbg(F,A), io:format(F,A)).
%%-define(dbg(F,A), ok).

%% -define(is_value(X), 
%%	(is_boolean(X)
%%	 orelse (X == undef) 
%%	 orelse is_number(X) 
%%	 orelse is_binary(X))).

file(Filename) ->
    file(Filename,[]).

file(Filename, Opts) ->
    case scad:parse_file(Filename) of
	{ok, Stmts} ->
	    lint(Stmts, [{filename,Filename}|Opts]);
	Error ->
	    Error
    end.

string(Data) ->
    string(Data,[]).

string(Data,Opts) when is_list(Opts) ->
    case scad:parse_string(Data) of
	{ok, Stmt} ->
	    lint(Stmt, [{filename, "*string*"}|Opts]);
	Error ->
	    Error
    end.

lint(Stmts) ->
    lint(Stmts, []).

lint(Stmts, Opts) when is_list(Stmts), is_list(Opts) ->
    Filename = proplists:get_value(filename, Opts, "unknown"),
    Stmts1 = [{file,Filename}|Stmts]++[eof],
    Stmt1 = input(Stmts1,[],[]) ++ get_defs(Opts),
    stmt_list(Stmt1, #{}).

input([{include,_Line,Filename}|Stmts],Acc,Fs) ->
    case scad:parse_file(Filename) of
	{ok, Stmts1} ->
	    Stmts2 = [{file,Filename}|Stmts1]++[eof|Stmts],
	    input(Stmts2,Acc,Fs);
	Error ->
	    io:format("scad_lint: include <~s> error: ~p~n", 
		      [Filename,Error]),
	    input(Stmts,Acc,Fs)
    end;
input([{use,_Line,Filename}|Stmts],Acc,Fs) ->
    case scad:parse_file(Filename) of
	{ok, Stmts1} ->
	    Stmts2 = filter_use(Stmts1),
	    Stmts3 = [{file,Filename}|Stmts2]++[eof|Stmts],
	    input(Stmts3,Acc,Fs);
	Error ->
	    io:format("scad_lint: use <~s> error: ~p~n", 
		      [Filename,Error]),
	    input(Stmts,Acc,Fs)
    end;
input([{file,Filename}|Stmts],Acc,Fs) ->
    input(Stmts,[{file,Filename}|Acc],[Filename|Fs]);
input([eof],Acc,[_]) ->  %% last pop (followed by -D assignements)
    lists:reverse(Acc);
input([eof|Stmts],Acc,[_|Fs1=[Filename|_Fs]]) ->
    input(Stmts,[{file,Filename}|Acc],Fs1);
input([Stmt|Stmts],Acc,Fs) ->
    input(Stmts,[Stmt|Acc],Fs).
%%input([], Acc,_Fs) ->
%%    lists:reverse(Acc).

%% fixme: handle {use, Filename} and {input,Filename}
filter_use([empty|Stmts]) ->
    filter_use(Stmts);
filter_use([{assign,_Ln,_Var,_Expr}|Stmts]) ->
    filter_use(Stmts);
filter_use([{mcall,_Ln,_Tags,_Id,_Args,_ChildStmt}|Stmts]) ->
    filter_use(Stmts);
filter_use([{block,_Ln,Stmts}|Stmts1]) ->
    filter_use(Stmts) ++ filter_use(Stmts1);
filter_use([{'if',_Ln,_Cond,_Then,_Else}|Stmts]) ->
    filter_use(Stmts);
filter_use([{'if',_Ln,_Cond,_Then}|Stmts]) ->
    filter_use(Stmts);
filter_use([Stmt={module,_Ln,_Name,_Params,_Stmt1}|Stmts]) ->
    [Stmt|filter_use(Stmts)];
filter_use([Stmt={function,_Ln,_Name,_Params,_Expr}|Stmts]) ->
    [Stmt|filter_use(Stmts)];
filter_use([]) ->
    [].

stmt_list(Stmts,St) when is_list(Stmts), is_map(St) ->
    Bound = scad_eval:default_scope(),
    case stmt_list_(Stmts,sets:new(),sets:new(),Bound,[],St) of
	{Stmts1,_Use1,_Set1,_Bound1} ->
	    {ok, Stmts1}
    end.

stmt_list_([{assign,_Line,{id,_,Var},Expr}|Stmts],Use,Set,Bound,Acc,St) ->
    Set2 = sets:add_element(Var,Set),
    case remove_assignments_(Var,undefined,Stmts,[]) of
	{undefined,_} ->
	    %% this is the last assignment of Var, check if Var is in the scope
	    %% if this is the case then move the assignment first!?
	    %% or maybe before the first use of Var
	    case sets:is_element(Var, Use) of
		true ->
		    {Expr1,Use2} = expr(Expr,Use,Bound),
		    stmt_list_(Stmts,Use2,Set2,
			       Bound, %% Bound#{Var => Expr1},
			       Acc++[{'=',Var,Expr1}],
			       St);
		false ->
		    {Expr1,Use2} = expr(Expr,Use,Bound),
		    stmt_list_(Stmts,Use2,Set2,
			       Bound, %% Bound#{Var => Expr1},
			       [{'=',Var,Expr1}|Acc],
			       St)
	    end;
        {{assign,_Line2,_,Expr1},Stmts1} ->
	    {Expr2,Use2} = expr(Expr1,Use,Bound),
	    stmt_list_(Stmts1,Use2,Set2,
		       Bound, %% Bound#{Var=>Expr2},
		       [{'=',Var,Expr2}|Acc], St)
    end;
stmt_list_([Stmt={file,Filename}|Stmts],Use,Set,Bound,Acc,St) ->
    stmt_list_(Stmts,Use,Set,Bound,[Stmt|Acc],St#{filename=>Filename});
stmt_list_([Stmt|Stmts],Use,Set,Bound,Acc,St) ->
    {Stmt1,Use1,Set1,Bound1} = stmt(Stmt,Use,Set,Bound,St),
    stmt_list_(Stmts,Use1,Set1,Bound1,[Stmt1|Acc],St);
stmt_list_([],Use,Set,Bound,Acc,_St) ->
    {lists:reverse(Acc),Use,Set,Bound}.

stmt(Stmt,Use,Set,Bound,St) ->
    case Stmt of
	empty ->
	    {empty,Use,Set,Bound};
	{block,_Line,Stmts} ->
	    {Stmts1,Use1,Set1,Bound1} = stmt_list_(Stmts,Use,Set,Bound,[],St),
	    {{block,Stmts1},Use1,Set1,Bound1};
	{module,_Line,{id,_,Name},Params,Stmt1} ->
	    {Params1,Ps,Use1,Bound1,Map} = params(Params, Bound),
	    ?dbg("params: ps=~p\n", [sets:to_list(Ps)]),
	    ?dbg("params: use=~p\n", [sets:to_list(Use1)]),
	    %% Bound1 = unbind_params(Params1, Bound),
	    ?dbg("Stmt1=~p\n", [Stmt1]),
	    {Stmt2,Use2,Set2,_Bound2} = stmt(Stmt1,Use1,sets:new(),Bound1,St),
	    ?dbg("stmt: use=~p\n", [sets:to_list(Use2)]),
	    ?dbg("stmt: set=~p\n", [sets:to_list(Set2)]),
	    Scope = sets:subtract(sets:subtract(Use2, Ps), Set2),
	    Mod = #mod{name=Name,use=use_list(Scope),params=Params1,
		       map=Map,stmt=Stmt2},
	    NBound = set_module(Name, Mod, Bound),
	    {Mod,sets:union(Use, Scope),Set,NBound};
	{function,_Line,{id,_,Name},Params,Expr} ->
	    {Params1,Ps,Use1,Bound1,Map} = params(Params, Bound),
	    %% Bound1 = unbind_params(Params1, Bound),
	    {Expr1,Use2} = expr(Expr,Use1,Bound1),
	    Scope = sets:subtract(Use2, Ps),  %% scope varaibles
	    Func = #func{name=Name,use=use_list(Scope),params=Params1,
			 map=Map,expr=Expr1},
	    NBound = set_function(Name, Func, Bound),
	    {Func,sets:union(Use, Scope),Set,NBound};
	%% FIXME move all mcalls efter lookup!!! below
	%% let (v1 = range1, ...  vn = rangen) stmt
	{mcall,_Line,Tags,{id,_,"let"},Args,ChildStmt} ->
	    {Args1,Use1,Bound1} = let_args(Args,Use,Bound),
	    Vs = args(Args1),
	    {ChildStmt1,Use2,Set2,_Bound2} =
		stmt(ChildStmt,sets:new(),sets:new(),Bound1,St),
	    Scope = sets:subtract(sets:subtract(Use2, arg_set(Vs)), Set2),
	    Let = {'let',Tags,Args1,ChildStmt1},
	    {Let, sets:union(Use1, Scope),Set,Bound};
	%% assign (v1 = range1, ...  vn = rangen) stmt
	{mcall,_Line,Tags,{id,_,"assign"},Args,ChildStmt} ->
	    {Args1,Use1,Bound1} = let_args(Args,Use,Bound),
	    Vs = args(Args1),
	    {ChildStmt1,Use2,Set2,_Bound2} = 
		stmt(ChildStmt,sets:new(),sets:new(),Bound1,St),
	    Scope = sets:subtract(sets:subtract(Use2, arg_set(Vs)), Set2),
	    Let = {'let',Tags,Args1,ChildStmt1},
	    {Let,sets:union(Use1, Scope),Set,Bound};
	%% for (v1 = range1, ...  vn = rangen) stmt 
	{mcall,_Line,Tags,{id,_,"for"},Args,ChildStmt} ->
	    ?dbg("for: use = ~p\n", [sets:to_list(Use)]),
	    {Args1,Use1} = arg_list(Args,Use,Bound),
	    ?dbg("for: use1 = ~p\n", [sets:to_list(Use1)]),
	    Vs = args(Args1),
	    ?dbg("for: vs = ~p\n", [Vs]),
	    {ChildStmt1,Use2,Set2,_Bound1} =
		stmt(ChildStmt,sets:new(),sets:new(),Bound,St),
	    Scope = sets:subtract(sets:subtract(Use2, arg_set(Vs)), Set2),
	    For = {'for',Tags,Args1,ChildStmt1},
	    {For, sets:union(Use1, Scope),Set,Bound};
	%% [!#%*] id(Args) stmt
	{mcall,_Line,Tags,{id,_,ID},Args,ChildStmt} ->
	    %% FIXME: recursive calls (and forward calls)
	    case get_module(ID, Bound) of
		undefined ->
		    ?dbg("~s:~w unbound module ~p~n", 
			 [maps:get(filename,St), _Line, ID]),
		    {empty,Use,Set,Bound};
		#mod{params=Params,map=Map,stmt=_} ->
		    {Args1, Use1} = arg_list(Args,Use,Bound),
		    ?dbg("lint: mcall: id=~s args=~p, params=~p, map=~p, use=~p\n", 
			      [ID, Args1, Params, Map, sets:to_list(Use)]),
		    Args2 = if Params == ?vararg ->
				    Args1;
			       true ->
				    ArgsTuple = params_to_tuple(Params, Args1),
				    ?dbg("ArgsTuple=~p\n", [ArgsTuple]),
				    args_to_vec(Args1, Map, ArgsTuple)
			    end,
		    ?dbg("Args2 = ~p\n", [Args2]),
		    {ChildStmt1,_Use2,_Set2,_Bound} = 
			stmt(ChildStmt,Use1,sets:new(),Bound,St),
		    %% Bind Sets2 ?
		    Mcall = {mcall,ID,Tags,Args2,ChildStmt1},
		    {Mcall,Use,Set,Bound}
	    end;
	{'if', _Line, Cond, Then, Else} ->
	    {Cond1,Use1} = expr(Cond,Bound,Use),
	    {Then1,Use2,Set2,Bound1} = stmt(Then,Use1,Set,Bound,St),
	    {Else1,Use3,Set3,Bound2} = stmt(Else,Use2,Set2,Bound,St),
	    IF = {'if',Cond1,Then1,Else1},
	    Bound3 = maps:merge(Bound1, Bound2),
	    {IF, Use3, Set3,Bound3};
	{'if', _Line, Cond, Then} ->
	    {Cond1,Use1} = expr(Cond,Bound,Use),
	    {Then1,Use2,Set2,Bound2} = stmt(Then,Use1,Set,Bound,St),
	    IF = {'if',Cond1,Then1},
	    {IF, Use2, Set2, Bound2}
    end.

get_defs(Opts) ->
    get_defs(Opts, []).
get_defs([{d,Var,Value}|Opts], Defs) ->
    {ok, [{assign,_Ln,{id,_,"___"},Value1}]} = 
	scad:parse_string("___="++Value++";"),
    get_defs(Opts, [{assign,0,{id,0,Var},Value1}|Defs]);
get_defs([_|Opts], Defs) ->
    get_defs(Opts, Defs);
get_defs([], Defs) ->
    lists:reverse(Defs).

%%unbind_params([{Var,_}|Params], Bound) ->
%%    unbind_params(Params, maps:remove(Var, Bound));
%%unbind_params([Var|Params], Bound) ->
%%    unbind_params(Params, maps:remove(Var, Bound));
%%unbind_params([], Bound) ->
%%    Bound.

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

%%
%% Lint an expression and return a more condensed version
%% totgether with a variables usage set
%%
expr(Expr,Use,Bound) ->
    expr_(Expr,Use,Bound).
    
expr_(empty,Use,_Bound) ->
    {empty, Use};
expr_({id,_Ln,Var},Use,Bound) ->
    %% check if Var is a predefined constant
    case maps:get(Var, Bound, undef) of
	undef -> %% not a constant - we could have assignments here!?
	    {{var,Var}, sets:add_element(Var,Use)};
	Value ->
	    {Value, Use}
    end;
expr_({number,_Ln,Num},Use,_Bound) -> {Num, Use};
expr_({string,_Ln,Str},Use,_Bound) -> {Str, Use};
expr_({true,_Ln},Use,_Bound) ->  {true, Use};
expr_({false,_Ln},Use,_Bound) -> {false, Use};
expr_({undef,_Ln},Use,_Bound) -> {undef, Use};
expr_({vector,_ln,Elems},Use,Bound) ->
    {Elems1, Use1} = vector(Elems,Use,Bound),
    case lists:all(fun(X) -> ?is_value(X) end, Elems1) of
	true ->
	    {Elems1, Use1}; %% constant vector!
	false ->
	    {{vector,Elems1}, Use1}
    end;
expr_({range,_Ln,Start,End},Use,Bound) ->
    %% maybe expand some "small" cases? => vector
    {S,Use1} = expr(Start,Use,Bound),
    {E,Use2} = expr(End,Use1,Bound),
    {{range, {S, 1, E}} , Use2};
expr_({range,_Ln,Start,Inc,End},Use,Bound) ->
    %% maybe expand some "small" cases? => vector
    {S,Use1} = expr_(Start,Use,Bound),
    {I,Use2} = expr_(Inc,Use1,Bound),
    {E,Use3} = expr_(End,Use2,Bound),
    {{range, {S, I, E}},Use3};
expr_({function,_Ln,Params,Expr},Use,Bound) ->
    {Params1,Ps,Use1,Bound1,Map} = params(Params, Bound),
    %% Bound1 = unbind_params(Params1, Bound),
    {Expr1,Use2} = expr(Expr,Use1,Bound1),
    Scope = sets:subtract(Use2, Ps),  %% scope varaibles
    Func = #func{use=use_list(Scope),params=Params1,map=Map,expr=Expr1},
    {Func,sets:union(Use, Scope)};
expr_({'echo',_Ln,Args,Expr},Use,Bound) ->
    {Args1, Use1} =  arg_list(Args,Use,Bound),
    {Expr1, Use2} = expr(Expr,Use1,Bound),
    {{'echo',Args1,Expr1}, Use2};
expr_({'assert',Line,Args,Expr},Use,Bound) ->
    {Args1, Use1} =  arg_list(Args,Use,Bound),
    {Expr1, Use2} = expr(Expr,Use1,Bound),
    {{'assert',Line,Args1,Expr1},Use2};
expr_({'let',_Ln,Args,Expr},Use,Bound) ->
    {Args1,Use1,Bound1} = let_args(Args,Use,Bound),
    {Expr1, Use2} = expr_(Expr,Use1,Bound1),
    {{'let',Args1,Expr1}, Use2};
expr_({op,_Ln,call,Call,Args},Use,Bound) -> 
    {Call1,Use1} = call(Call,Use,Bound),
    {Args1, Use2} = arg_list(Args,Use1,Bound),
    Args2 = case Call1 of
		#func{name=_Name,params=?vararg,map=_Map} ->
		    Args1;
		#func{name=_Name,params=Params,map=Map} ->
		    ArgsTuple = params_to_tuple(Params, Args1),
		    ?dbg("ArgsTuple=~p\n", [ArgsTuple]),
		    args_to_vec(Args1, Map, ArgsTuple);
		_ ->
		    Args1
	    end,
    ?dbg("Args2 = ~p\n", [Args2]),
    %% partial eval if possible (builtin functions)
    try case {Call1,Args2} of
	    {#func{expr=Expr},[X]} when is_function(Expr,1) ->
		Expr(X);
	    {#func{expr=Expr},[X,Y]} when is_function(Expr,1) ->
		Expr([X,Y]) end of
	Value ->
	    {Value,Use1}
    catch
	error:_ ->
	    case Call1 of
		#func{} ->
		    {{op,call,Call1,Args2},Use2};
		_ ->
		    {{op,call,Call1,Args1},Use2}
	    end
    end;
expr_({op,_Ln,Op,Arg1},Use,Bound) -> 
    {A1,Use1} = expr_(Arg1,Use,Bound),
    try case Op of
	    '+' -> A1;
	    '-' -> scad_eval:negate(A1);
	    '!' -> not scad_eval:bool(A1);
	    _ -> undef
	end of
	A2 ->
	    {A2,Use1}
    catch
	error:_ ->
	    {{op,Op,A1},Use1}
    end;
expr_({op,_Ln,member,Arg1,{id,_,ID}},Use,Bound) ->
    {A1,Use1} = expr_(Arg1,Use,Bound),
    Ind = case ID of
	      "x" -> [0];
	      "y" -> [1];
	      "z" -> [2];
	      _ -> undef
	  end,
    try index(Ind, A1) of
	Value ->
	    {Value,Use1}
    catch
	error:_ ->
	    {{op,index,A1,Ind},Use1}
    end;
expr_({op,_Ln,Op,Arg1,Arg2},Use,Bound) ->
    {A1,Use1} = expr_(Arg1,Use,Bound),
    {A2,Use2} = expr_(Arg2,Use1,Bound),
    try case Op of
	    '+' -> scad_eval:add(A1,A2);
	    '-' -> scad_eval:subtract(A1,A2);
	    '*' -> scad_eval:multiply(A1, A2);
	    '/' -> scad_eval:divide(A1, A2);
	    '%' -> scad_eval:reminder(A1, A2);
	    '==' when ?is_value(A1), ?is_value(A2) -> scad_eval:eq(A1, A2);
	    '!=' when ?is_value(A1), ?is_value(A2) -> scad_eval:neq(A1,A2);
	    '>' when ?is_value(A1), ?is_value(A2) -> scad_eval:gt(A1,A2);
	    '>=' when ?is_value(A1), ?is_value(A2) -> scad_eval:gte(A1,A2);
	    '<' when ?is_value(A1), ?is_value(A2) -> scad_eval:lt(A1,A2);
	    '<='  when ?is_value(A1), ?is_value(A2) -> scad_eval:lte(A1,A2);
	    '&&' -> scad_eval:bool(A1) andalso scad_eval:bool(A2);
	    '||' -> scad_eval:bool(A1) orelse scad_eval:bool(A2);
	    '^' -> math:pow(A1,A2);
	    index -> index(A2, A1)
	end of
	A3 ->
	    {A3,Use2}
    catch
	error:_ ->
	    {{op,Op,A1,A2},Use2}
    end;
expr_({op,_Ln,'?',Cond,Then,Else},Use,Bound) ->
    %% fixme: partial evaluation
    {Cond1,Use1} = expr_(Cond,Use,Bound),
    {Then1,Use2} = expr_(Then,Use1,Bound),
    {Else1,Use3} = expr_(Else,Use2,Bound),
    {{op,'?',Cond1,Then1,Else1},Use3}.

call({id,_Ln,Var}, Use, Bound) ->
    case maps:get(Var, Bound, undefined) of
	undefined ->
	    case get_function(Var, Bound) of
		undefined ->
		    {undef, Use};
		Fn = #func{} ->
		    {Fn, Use}
	    end;
	Fn=#func{} ->  %% anon function!
	    {Fn, sets:add_element(Var,Use)};
	_ -> %% undef or ?
	    {{var,Var}, sets:add_element(Var,Use)}
    end;
call(Expr, Use, Bound) ->
    expr(Expr,Use,Bound).


vector([E|Es],Use,Bound) ->
    {E1,Use1} = vector_element(E, Use,Bound),
    {Es1,Use2} = vector(Es, Use1,Bound),
    {[E1|Es1], Use2};
vector([], Use, _Bound) ->
    {[], Use}.

vector_element(Element,Use,Bound) ->
    case Element of
	{lc_for,_Ln,Args,Element1} ->
	    {Args1,Use1,Bound1} = let_args(Args,Use,Bound),
	    Vs = args(Args1),
	    {Element2, Use2} = vector_element(Element1,sets:new(),Bound1),
	    Scope = sets:subtract(Use2, arg_set(Vs)),
	    {{lc_for,Args1,Element2}, sets:union(Use1, Scope)};

	{lc_let,_Ln,Args,Element1} ->
	    {Args1,Bound1,Use1} = let_args(Args,Use,Bound),
	    Vs = args(Args1),
	    {Element2, Use2} = vector_element(Element1,sets:new(),Bound1),
	    Scope = sets:subtract(Use2, arg_set(Vs)),
	    {{lc_let,Args1,Element2}, sets:union(Use1, Scope)};

	{lc_each,_Ln, Element1} ->
	    {Element2,Use1} = vector_element(Element1, Use, Bound),
	    {{lc_each, Element2}, Use1};

	{lc_forc,_Ln,Init,Cond,Update,Element1} ->
	    {Init1,Use1,Bound1} = let_args(Init,Use,Bound),
	    {Cond1, Use2} = expr(Cond, Use1,Bound1),
	    {Update1,Use3,Bound2} = let_args(Update,Use2,Bound1),
	    Vs = args(Init1),
	    {Element2, Use4} = vector_element(Element1,sets:new(),Bound2),
	    Scope = sets:subtract(Use4, arg_set(Vs)),
	    {{lc_forc,Init1,Cond1,Update1,Element2}, sets:union(Use3, Scope)};

	{lc_if,_Ln,Cond,Then} ->
	    {Cond1,Use1} = expr(Cond,Use,Bound),
	    {Then1,Use2} = vector_element(Then,Use1,Bound),
	    {{lc_if,Cond1,Then1}, Use2};
	    
	{lc_if,_Ln,Cond,Then,Else} ->
	    {Cond1,Use1} = expr(Cond,Use,Bound),
	    {Then1,Use2} = vector_element(Then,Use1,Bound),
	    {Else1,Use3} = vector_element(Else,Use2,Bound),
	    {{lc_if,Cond1,Then1,Else1}, Use3};
	_ ->
	    expr(Element,Use,Bound)
    end.


%% sequential bind of
%%let_args(Args) ->
%%    let_args(Args, sets:new()).
let_args(Args,Use,Bound) ->
    let_args(Args,Use,Bound,[]).

let_args([{'=',_Ln0,{id,_Ln,Var},Expr}|Args],Use,Bound,Acc) ->
    {Expr1, Use1} = expr(Expr,Use,Bound),
    let_args(Args, Use1,  Bound#{Var=>Expr1},[{'=',Var,Expr1}|Acc]);
let_args([],Use,Bound,Acc) ->
    {lists:reverse(Acc),Use,Bound}.

params(?vararg, Bound) ->
    {[], sets:new(), [], [], Bound, 1, #{}};
params(Params,Bound) ->
    params(Params, sets:new(), sets:new(), Bound, 1, #{}, []).
%%params(Params,Use,Bound) ->
%%    params(Params, sets:new(), Use, Bound, 1, #{}, []).

params([{'=',_Ln0,{id,_Ln,Var},Expr}|Args], Ps, Use, Bound, I, Map, Acc) ->
    {Expr1,Use1} = expr(Expr,Use,Bound),
    params(Args,  sets:add_element(Var, Ps), Use1, 
	   Bound, %% Bound#{Var => Expr1},
	   I+1,
	   Map#{Var => I}, [{'=',Var,Expr1}|Acc]);
params([{id,_Ln,Var}|Args], Ps, Use, Bound, I, Map, Acc) ->
    params(Args,  sets:add_element(Var, Ps), Use,
	   Bound, %%Bound#{Var => undef},
	   I+1, 
	   Map#{Var => I}, [Var|Acc]);
params([], Ps, Use, Bound, _I, Map, Acc) ->
    {lists:reverse(Acc),Ps,Use,Bound,Map}.

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

arg_list(Args,Use,Bound) ->
    arg_list(Args,Use,Bound,[]).

arg_list([{'=',_Ln0,{id,_Ln,Var},Arg}|Args],Use,Bound,Acc) ->
    {Arg1,Use1} = expr(Arg,Use,Bound),
    arg_list(Args, Use1, Bound,[{'=',Var,Arg1}|Acc]);
arg_list([Arg|Args], Use,Bound, Acc) ->
    {Arg1,Use1} = expr(Arg,Use,Bound),
    arg_list(Args, Use1,Bound,[Arg1|Acc]);
arg_list([], Use,_Bound,Acc) ->
    {lists:reverse(Acc), Use}.

%% return list of variable names from set of variables
-spec use_list(sets:set()) -> list().
use_list(Use) ->
    sets:to_list(Use).

arg_set(Args) ->
    sets:from_list(args(Args)).

%% argument list to variable list
args([{'=', ID,_Expr} | Args]) -> [ID | args(Args)];
args([ID|Args]) -> [ID | args(Args)];
args([]) -> [].

index([Ind], Vec) when is_integer(Ind), is_list(Vec) ->
    lists:nth(Ind+1, Vec).

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
