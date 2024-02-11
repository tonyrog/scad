%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Tree structure for OpenSCAD objects
%%% @end
%%% Created :  6 Feb 2024 by Tony Rogvall <tony@rogvall.se>

-module(scad_object).
-export([create_tree/1]).
-export([print_tree/1, print_tree/2]).
-export([scope/2,
	 cube/1,cube/2,cube/3,
	 sphere/1,sphere/2,sphere/3,
	 cylinder/2,cylinder/3,cylinder/4,
	 cone/2,cone/3,cone/4,
	 intersection/1,intersection/2,
	 difference/1,difference/2,
	 union/1,union/2,
	 color/2,
	 rotate/2,rotate/3,
	 translate/2,translate/3,
	 scale/2,scale/3]).
-export([file/1, demo/0,demo_car/0,demo_car_h/0]).
-compile(export_all).
-include("scad.hrl").

-type node_id() :: non_neg_integer().
-type node_type() :: none | scope |cube | sphere | cylinder |
		     union | difference | intersection |
		     rotate | scale | translate | 
		     color.
-type node_color() :: {R::float(),G::float(),B::float()}.
-type node_param() :: {Key::atom(), Value::term()}.
-type vertex() :: {X::float(),Y::float(),Z::float()}.
-type id_gen() :: counters:counters_ref().
-define(COLOR, ?GOLD).

-record(node,
	{
	 type = none :: node_type(),
	 id = 0      :: node_id(),  %% root get node id = 0
	 parent = -1 :: node_id(),  %% no parent (for root)
	 wid = -1    :: integer(),  %% wings object id (when applicable)
	 vs = []     :: [vertex()], %% original vertices (when applicable)
	 %% combined transformation matrix parents
	 ctm = identity    ::identity | e3d_mat:compact_matrix(),
	 %% local transformation matrix
	 ltm = identity :: identity | e3d_mat:compact_matrix(),
	 color = ?GOLD :: node_color(),
	 name = ""     :: string(),
	 params = [] :: [node_param()],
	 children = [] :: [node_id()]
	}).

-type tree() :: #{node_id() => #node{}}.

-define(is_children(Obj),
	is_record((Obj),object) orelse
        ((Obj) =:= []) orelse
	(is_list((Obj)) andalso is_record(hd(Obj),object))).


demo() ->
    Tree = union(
	     [
	      cylinder(4.0,1.0,[{center,true},{'$fn',100}]),
	      rotate(90,[1.0,0.0,0.0],
		     [cylinder(4.0,0.9,[{center,true},{'$fn',100}])])
	     ]),
    create_tree(Tree).

demo_car() ->
    scad_wings:start_wings(),
    scad_wings:clear(),
    Car = car:car(),
    create_tree(Car).

demo_car_h() ->
    scad_wings:start_wings(),
    scad_wings:clear(),
    Car= car_h:car(),
    {Root, Tree} = create_tree(Car),
%%    {Root, Tree}.
    BodyID = find_node_by_name("body", Tree),
    timer:sleep(3000),
    lists:foreach(
      fun(A) ->
	      timer:sleep(100),
	      _ = update_node(Tree, BodyID, [{a, [A,0.0,0.0]}])
      end, lists:seq(270, 180, -5)),
    {Root,Tree}.

file(Filename) ->
    Objs = scad_eval:file(Filename),
    scad_wings:start_wings(),
    scad_wings:clear(),
    {Root, Tree} = create_tree(Objs),
    {Root, Tree}.


%% object constructors
scope(Params,Children) when is_list(Params), ?is_children(Children) ->
    #object{type=scope,params=Params,children=Children}.

cube([Sx,Sy,Sz]) 
  when is_number(Sx), is_number(Sy), is_number(Sz) ->
    cube_(Sx,Sy,Sz,[],[]).

cube(X,Params) when is_number(X), is_list(Params) ->
    X1 = float(X),
    cube_(X1,X1,X1,Params,[]);
cube([Sx,Sy,Sz],Params) 
  when is_number(Sx), is_number(Sy), is_number(Sz) ->
    cube_(Sx,Sy,Sz,Params,[]);
cube([Sx,Sy,Sz],Children) 
  when is_number(Sx), is_number(Sy), is_number(Sz),
       ?is_children(Children) ->
    cube_(Sx,Sy,Sz,[],Children).

cube(X,Params,Children) when 
      is_number(X), is_list(Params), 
      is_list(Params), ?is_children(Children) ->
    X1 = float(X),
    cube_(X1,X1,X1,Params,Children);
cube([Sx,Sy,Sz],Params,Children) 
  when is_number(Sx), is_number(Sy), is_number(Sz),
       is_list(Params), ?is_children(Children) ->
    cube_(Sx,Sy,Sz,Params,Children).

cube_(Sx,Sy,Sz,Params,Children) ->
    Vec = [Sx,Sy,Sz],
    #object{type=cube,params=[{size,Vec}|Params],children=Children}.

sphere(Params) when is_list(Params) -> 
    sphere_(1.0,Params,[]);
sphere(R) when is_number(R) ->
    sphere_(float(R), [], []).

sphere(R,Children) when is_number(R), ?is_children(Children) ->
    sphere_(float(R), [], Children);
sphere(R,Params) when is_number(R), is_list(Params) ->
    sphere_(float(R), Params, []).

sphere(R,Params,Children) when 
      is_number(R), is_list(Params), ?is_children(Children) ->
    sphere_(float(R), Params, Children).

sphere_(R,Params,Children) -> 
    #object{type=sphere,params=[{r,R}|Params],children=Children}.


cylinder(H,R) when is_number(H), is_number(R) ->
    RR = float(R),
    cylinder_(float(H),RR,RR,[],[]).

cylinder(H,R1,R2) when is_number(H), is_number(R1), is_number(R2) ->
    cylinder_(float(H),float(R1),float(R2),[],[]);
cylinder(H,R,Params) when is_number(H), is_number(R), is_list(Params) ->
    RR = float(R),
    cylinder_(float(H),RR,RR,Params,[]);
cylinder(H,R,Children) when is_number(H), is_number(R),?is_children(Children) ->
    RR = float(R),
    cylinder_(float(H),RR,RR,[],Children).

cylinder(H,R1,R2,Params) when 
      is_number(H),is_number(R1), is_number(R2), is_list(Params) ->
    cylinder_(float(H),float(R1),float(R2),Params,[]);
cylinder(H,R,Params,Children) when 
      is_number(H),is_number(R), is_list(Params), ?is_children(Children) ->
    RR = float(R),
    cylinder_(float(H),RR,RR,Params,Children).

cone(H,R) when is_number(H), is_number(R) ->
    cylinder_(float(H),0.0,float(R),[],[]).

cone(H,R,Params) when is_number(H), is_number(R), is_list(Params) ->
    cylinder_(float(H),0.0,float(R),Params,[]);
cone(H,R,Children) when is_number(H), is_number(R), ?is_children(Children) ->
    cylinder_(float(H),0.0,float(R),[],Children).

cone(H,R,Params,Children) when
      is_number(H), is_number(R), is_list(Params), ?is_children(Children) ->
    cylinder_(float(H),0.0,float(R),Params,Children).


cylinder_(H,R1,R2,Params,Children) ->
    #object{type=cylinder,
	    params=[{h,H},{r1,R1},{r2,R2}|Params],children=Children}.
    

intersection(Children) when ?is_children(Children) ->
    intersection_([],Children).
intersection(Params,Children) when is_list(Params), ?is_children(Children) ->
    intersection_(Params,Children).
intersection_(Params,Children) when is_list(Params), ?is_children(Children) ->
    #object{type=intersection,params=Params,children=Children}.

difference(Children) -> difference_([],Children).
difference(Params,Children) when is_list(Params), ?is_children(Children) ->
    difference_(Params,Children).
difference_(Params,Children) when is_list(Params), ?is_children(Children) ->
    #object{type=difference,params=Params,children=Children}.

union(Children) when ?is_children(Children) -> 
    union_([],Children).
union(Params,Children) when is_list(Params), ?is_children(Children) ->
    union_(Params,Children).
union_(Params,Children) ->
    #object{type=union,params=Params,children=Children}.

color(Color,Children) when is_list(Color), ?is_children(Children) ->
    color_(Color,Children).
color_(Color,Children) ->
    #object{type=color,params=[{c,(Color)}],children=Children}.

rotate([Ax,Ay,Az],Children) 
  when ?is_children(Children),
       is_number(Ax), is_number(Ay), is_number(Az) ->
    A = [float(Ax),float(Ay),float(Az)],
    #object{type=rotate,params=[{a,A}],children=(Children)};
rotate(A,Children) when
      ?is_children(Children), is_number(A) ->
    #object{type=rotate,params=[{a,A}],children=(Children)}.
rotate([Ax,Ay,Az],Params,Children) 
  when is_number(Ax), is_number(Ay), is_number(Az), is_list(Params),
       ?is_children(Children) ->
    A = [float(Ax),float(Ay),float(Az)],
    #object{type=rotate,params=[{a,A}|Params],children=(Children)};
rotate(A,[Vx,Vy,Vz],Children) when
      ?is_children(Children),
      is_number(A), is_number(Vx), is_number(Vy), is_number(Vz) ->
    V = [float(Vx),float(Vy),float(Vz)],
    #object{type=rotate,params=[{a,A},{v,V}],children=(Children)}.

translate([Vx,Vy,Vz],Children) when 
      ?is_children(Children),
      is_number(Vx), is_number(Vy), is_number(Vz) ->
    V = [float(Vx),float(Vy),float(Vz)],
    translate_(V,[],Children).
translate([Vx,Vy,Vz],Params,Children) when
      ?is_children(Children), is_list(Params),
      is_number(Vx), is_number(Vy), is_number(Vz) ->
    V = [float(Vx),float(Vy),float(Vz)],
    translate_(V,[],Children).

translate_(Vec,Params,Children) ->
    #object{type=translate,params=[{v,(Vec)}|Params],children=(Children)}.
    
scale(Vec,Children) ->
    scale(Vec,[],Children).
scale([Vx,Vy,Vz],Params,Children) when
      is_number(Vx), is_number(Vy), is_number(Vz), is_list(Params) ->
    Vec = [float(Vx),float(Vy),float(Vz)],
    #object{type=scale,params=[{v,(Vec)}|Params],children=(Children)}.


-spec create_tree(#object{}|[#object{}]) -> 
	  {RootID::node_id(),Tree::#{node_id() => #node{}}}.

create_tree(Obj) when is_record(Obj,object) -> 
    %% one root
    create_node(Obj, #{}, idgen_new(), -1, default_scope([]));
create_tree(Objs) when is_list(Objs), is_record(hd(Objs),object) -> 
    %% multiple roots (and a dummy root)
    create_node(#object{type=none,params=[],children=Objs}, 
		#{}, idgen_new(), -1, 
		default_scope([])).

default_scope(Env) when is_list(Env) ->
    #{ color => ?COLOR, 
       ctm => e3d_mat:identity(), 
       params => maps:from_list(Env) }.

-spec create_node(#object{},Tree::tree(),IDGen::id_gen(),node_id(),
		  Scope::#{}) -> {node_id(),tree()}.
create_node(#object{type=Type,params=Params,children=Children},
	    Tree, IDGen, ParentID,Scope) ->
    ID = idgen_next(IDGen),
    Name = proplists:get_value(name, Params, ""),
    case Type of
	none -> 
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    N = #node{type=none,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	scope ->
	    #{ params := Ps, ctm := Ctm, color := Color } = Scope,
	    Ps1 = maps:from_list(Params),
	    Scope1 = Scope#{params => maps:merge(Ps, Ps1)},
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope1),
	    N = #node{type=scope,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	color ->
	    Color1 = scad_eval:color(Params),
	    #{ ctm := Ctm } = Scope,
	    Scope1 = Scope#{color => Color1},
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope1),
	    N = #node{type=color,id=ID,parent=ParentID,
		      color=Color1,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	union ->
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    %% create a group object here?
	    N = #node{type=union,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	difference ->
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    %% create a group object here?
	    N = #node{type=difference,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	intersection ->
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    %% create a group object here?
	    N = #node{type=intersection,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	scale ->
	    Ltm = scale_ltm(proplists:get_value(v, Params, undef)),
	    #{ ctm := Ctm, color := Color } = Scope,
	    Ctm1 = e3d_mat:mul(Ctm, Ltm),
	    Scope1 = Scope#{ctm => Ctm1},
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope1),
	    N = #node{type=Type,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm1,ltm=Ltm,
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	translate ->
	    Ltm = translate_ltm(proplists:get_value(v, Params, undef)),
	    #{ ctm := Ctm, color := Color } = Scope,
	    Ctm1 = e3d_mat:mul(Ctm, Ltm),
	    Scope1 = Scope#{ctm => Ctm1},
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope1),
	    N = #node{type=Type,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm1, ltm=Ltm,
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	rotate ->
	    Ltm = rotate_ltm(proplists:get_value(a, Params, undef),
			     proplists:get_value(v, Params, undef)),
	    #{ ctm := Ctm, color := Color } = Scope,
	    Ctm1 = e3d_mat:mul(Ctm, Ltm),
	    Scope1 = Scope#{ ctm => Ctm1 },
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope1),
	    N = #node{type=Type,id=ID,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm1, ltm=Ltm,
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	cube ->
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    {WID,Vs} = scad_wings:r_cube_(Params, [], Scope),
	    N = #node{type=cube,id=ID,wid=WID,vs=Vs,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	sphere ->
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    {WID,Vs} = scad_wings:r_sphere_(Params, [], Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    N = #node{type=sphere,id=ID,wid=WID,vs=Vs,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}};
	cylinder ->
	    {IDs,Tree1} = create_nodes(Children,Tree,IDGen,ID,Scope),
	    {WID,Vs} = scad_wings:r_cylinder_(Params, [], Scope),
	    #{ ctm := Ctm, color := Color } = Scope,
	    N = #node{type=cylinder,id=ID,wid=WID,vs=Vs,parent=ParentID,
		      color=Color,name=Name,
		      ctm=Ctm,ltm=e3d_mat:identity(),
		      params=Params,children=IDs},
	    {ID,Tree1#{ID => N}}
    end.

create_nodes([],Tree,_IDGen,_ParentID,_Scope) ->
    {[],Tree};
create_nodes(Obj,Tree,IDGen,ParentID,Scope) 
  when is_record(Obj,object) ->
    create_nodes([Obj],Tree,IDGen,ParentID,Scope);
create_nodes([Obj|Objs],Tree,IDGen,ParentID,Scope) -> 
    {ID,Tree1} = create_node(Obj,Tree,IDGen,ParentID,Scope),
    {IDs,Tree2} = create_nodes(Objs,Tree1,IDGen,ParentID,Scope),
    {[ID|IDs],Tree2}.


-spec update_node(Tree::tree(),ID::node_id(),
		  Params::[{Key::atom(), Value::term()}]) ->
	  tree().

update_node(Tree,ID,Params) 
  when is_map(Tree), is_integer(ID), is_list(Params) ->
    N = maps:get(ID, Tree),
    %% extract Name
    case N#node.type of
	node ->
	    %% do not (yet) update children
	    N1 = N#node{params=Params},
	    maps:put(ID, N1, Tree);
	scope ->
	    N1 = N#node{params=Params},
	    Scope = #{ params => maps:from_list(Params) },
	    Tree1 = maps:put(ID, N1, Tree),
	    update_children(Tree1, N#node.children, Scope);
	color ->
	    Color1 = scad_eval:color(Params),
	    N1 = N#node { params = Params, color = Color1 },
	    Tree1 = maps:put(ID, N1, Tree),
	    update_children(Tree1, N1#node.children, #{ color => Color1 });
	union ->
	    %% FIXME: do not (yet) update children
	    %% children should be collapsed! (all vertices collected!)
	    N1 = N#node{params=Params},
	    maps:put(ID, N1, Tree);
	difference ->
	    %% FIXME: do not (yet) update children
	    N1 = N#node{params=Params},
	    maps:put(ID, N1, Tree);
	intersection ->
	    %% FIXME: do not (yet) update children
	    N1 = N#node{params=Params},
	    maps:put(ID, N1, Tree);
	scale ->
	    Ltm = scale_ltm(proplists:get_value(v, Params, undef)),
	    transform_node(Tree, N, ID, Params, Ltm);
	translate ->
	    Ltm = translate_ltm(proplists:get_value(v, Params, undef)),
	    transform_node(Tree, N, ID, Params, Ltm);
	rotate ->
	    Ltm = rotate_ltm(proplists:get_value(a, Params, undef),
			     proplists:get_value(v, Params, undef)),
	    transform_node(Tree, N, ID, Params, Ltm)
	%% FIXME: update cube/sphere/cylinder this normally require
	%%        the object beeing rebuilt
    end.

scale_ltm(undef) ->
    e3d_mat:scale({1.0, 1.0, 1.0});
scale_ltm([X,Y,Z]) ->
    e3d_mat:scale({float(X),float(Y),float(Z)});
scale_ltm([X,Y]) ->
    e3d_mat:scale({float(X),float(Y),1.0});
scale_ltm([X]) ->
    e3d_mat:scale({float(X),1.0,1.0}).

translate_ltm(undef) ->
    e3d_mat:translate({0.0, 0.0, 0.0});
translate_ltm([X,Y,Z]) ->
    e3d_mat:translate({float(X),float(Y),float(Z)});
translate_ltm([X,Y]) ->
    e3d_mat:translate({float(X),float(Y),0.0});
translate_ltm([X]) ->
    e3d_mat:translate({float(X),0.0,0.0}).

rotate_ltm(A, undef) when is_number(A) ->
    RAv = rad_vec({0.0,0.0,float(A)}),
    e3d_mat:rotate_from_euler_rad(RAv);
rotate_ltm([Ax,Ay,Az], undef) 
  when is_number(Ax), is_number(Ay), is_number(Az) ->
    RAv = rad_vec({float(Ax),float(Ay),float(Az)}),
    e3d_mat:rotate_from_euler_rad(RAv);
rotate_ltm([Ax,Ay], undef) 
  when is_number(Ax), is_number(Ay) ->
    RAv = rad_vec({float(Ax),float(Ay),0.0}),
    e3d_mat:rotate_from_euler_rad(RAv);
rotate_ltm([Ax], undef) 
  when is_number(Ax) ->
    RAv = rad_vec({float(Ax),0.0,0.0}),
    e3d_mat:rotate_from_euler_rad(RAv);
rotate_ltm(A, [Vx,Vy,Vz]) when
      is_number(A), is_number(Vx), is_number(Vy), is_number(Vz) ->
    e3d_mat:rotate(A, [float(Vx),float(Vy),float(Vz)]).
    

%% called on scale/translate/rotate nodes,
%% the node it self has not wings object so no update needed
transform_node(Tree, N, ID, Params, Ltm) ->
    P = maps:get(N#node.parent, Tree),
    Ctm = e3d_mat:mul(P#node.ctm, Ltm),
    N1 = N#node { params = Params, ctm = Ctm, ltm = Ltm },
    Tree1 = maps:put(ID, N1, Tree),
    update_children(Tree1, N1#node.children, #{ ctm => Ctm }).

update_children(Tree, [ID|IDs], Scope) ->
    Tree1 = update_child(Tree, ID, Scope),
    update_children(Tree1, IDs, Scope);
update_children(Tree, [], _Scope) ->
    Tree.

%% update child node with scope information
update_child(Tree, ID, Scope) ->
    N = maps:get(ID, Tree),
    Color = case maps:get(color, Scope, undefined) of
		undefined -> N#node.color;
		SColor -> SColor
	    end,
    case maps:get(ctm, Scope, undefined) of
	undefined ->
	    %% FIXME: collect all updates in a list!!!
	    N1 = set_node_color(N, Color),
	    Tree1 = maps:put(ID, N1, Tree),
	    update_children(Tree1, N#node.children, Scope);
	Ctm -> %% parent matrix
	    %% update local matrix
	    Ctm1 = e3d_mat:mul(Ctm, N#node.ltm),
	    Scope1 = Scope#{ ctm => Ctm1 },
	    N1 = set_node_color_and_ctm(N, Color, Ctm1),
	    Tree1 = maps:put(ID, N1, Tree),
	    update_children(Tree1, N#node.children, Scope1)
    end.

set_node_color(N=#node{wid=WID}, Color) when WID =:= -1 ->
    N#node { color=Color };
set_node_color(N=#node{wid=WID}, Color) ->
    scad_wings:w_color(Color, [WID]),
    N#node { color=Color }.

set_node_color_and_ctm(N=#node{wid=WID}, Color, Ctm) when WID =:= -1 ->
    N#node { color=Color, ctm=Ctm };
set_node_color_and_ctm(N=#node{wid=WID,vs=Vs}, Color, Ctm) ->
    scad_wings:w_transform([{WID,Ctm,Vs,Color}]),
    N#node { color=Color, ctm=Ctm }.


find_node_by_name(Name, Tree) ->
    I = maps:iterator(Tree),
    find_node_by_name_(Name, I).

find_node_by_name_(Name, I) ->
    case maps:next(I) of
	{ID, N, I1} ->
	    if N#node.name =:= Name ->
		    ID;
	       true -> find_node_by_name_(Name, I1)
	    end;
	none ->
	    false
    end.

-ifdef(not_used).
translate_node(Tree, ID, Vec=[Tx,Ty,Tz]) 
  when is_number(Tx), is_number(Ty), is_number(Tz) ->
    transform_node(Tree, ID, e3d_mat:translate(Vec)).

scale_node(Tree, ID, Vec=[Sx,Sy,Sz]) 
  when is_number(Sx), is_number(Sy), is_number(Sz) ->    
    transform_node(Tree, ID, e3d_mat:scale(Vec)).

rotate_node(Tree, ID, Vec=[Rx,Ry,Rz]) 
  when is_number(Rx), is_number(Ry), is_number(Rz) ->
    RadVec = rad_vec({Rx,Ry,Rz]),
    transform_node(Tree, ID, e3d_mat:rotate_from_euler_rad(RadVec)).
rotate_node(Tree, ID, A, Vec=[Rx,Ry,Rz]) 
  when is_number(A), is_number(Rx), is_number(Ry), is_number(Rz) ->    
    transform_node(Tree, ID, e3d_mat:rotate(A, Vec)).

transform_node(Tree, ID, Ltm) ->
    N = maps:get(ID, Tree),
    PID = N#node.parent,
    P = maps:get(PID, Tree),
    Ctm1 = e3d_mat:mul(P#node.ctm, Ltm),
    %% FIXME: transform all vertices vs in the node
    %% update all children !
    N1 = N#node { ctm = Ctm1, ltm = Ltm },
    maps:put(ID, N1, Tree).
-endif.

%% convert vec3d() from degree to radians
rad_vec({Rx,Ry,Rz}) when is_number(Rx),is_number(Ry),is_number(Rz) ->
    F = math:pi()/180,
    {F*Rx,F*Ry,F*Rz}.

idgen_new() ->
    counters:new(1, []).

idgen_next(IDGen) ->
    ok = counters:add(IDGen, 1, 1),
    counters:get(IDGen, 1).


print_tree({Root, Tree}) ->
    print_tree(Root, Tree).
print_tree(Root, Tree) ->
    io:format("Root: ~p~n", [Root]),
    print_tree_(Root, Tree, 0).

print_tree_(ID, Tree, Indent) ->
    case maps:find(ID, Tree) of
	{ok, N} ->
	    io:format("~s~p: ~p (~s)\n", 
		      [lists:duplicate(Indent, " "), 
		       ID, N#node.type, N#node.name]),
	    _ = [print_tree_(C, Tree, Indent+2) || C <- N#node.children],
	    ok;
	error ->
	    ok
    end.
