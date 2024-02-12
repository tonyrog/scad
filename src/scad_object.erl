%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Tree structure for OpenSCAD objects
%%% @end
%%% Created :  6 Feb 2024 by Tony Rogvall <tony@rogvall.se>

-module(scad_object).
-export([create_tree/1]).
-export([update_node/3, update_nodes/2]).
-export([find_node_by_name/2]).
-export([find_root/1, find_roots/1]).
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
-export([file/1, demo/0,demo_car/0,demo_car_h/0, demo_robot_h/0]).
%% -compile(export_all).
-include("scad.hrl").

-type node_id() :: non_neg_integer().
-type node_type() :: none | scope |cube | sphere | cylinder |
		     union | difference | intersection |
		     rotate | scale | translate | 
		     color.
-type node_color() :: {R::float(),G::float(),B::float()}.
-type node_param() :: {Key::atom(), Value::term()}.
%%-type vertex() :: {X::float(),Y::float(),Z::float()}.
-type id_gen() :: counters:counters_ref().
-define(COLOR, ?GOLD).

-record(node,
	{
	 type = none :: node_type(),
	 id = 0      :: node_id(),  %% root get node id = 0
	 parent = -1 :: node_id(),  %% no parent (for root)
	 wid = -1    :: integer(),  %% wings object id (when applicable)
	 vs = <<>>   :: binary(),   %% original vertices (when applicable)
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
    PosID = find_node_by_name("car_pos", Tree),
    CarID = find_node_by_name("car", Tree),
    AxelID = find_node_by_name("front_axle", Tree),
    TrackRadius = 200,
    timer:sleep(3000),
    lists:foreach(
      fun(A) ->
	      X = math:cos(A*math:pi()/180),
	      Y = math:sin(A*math:pi()/180),
	      timer:sleep(100),
	      _ = update_nodes(Tree, 
			       [{AxelID, [{a, [0,0,-20]}]},
				{PosID,[{v, [TrackRadius*X,0,TrackRadius*Y]}]},
				{CarID, [{a, [270,90-A,0]}]}
			       ]),
	      ok
      end, lists:seq(-90+1, 270-1, 2)),
    {Root,Tree}.

demo_robot_h() ->
    scad_wings:start_wings(),
    scad_wings:clear(),
    Robot = robot_h:model(),
    {Root, Tree} = create_tree(Robot),
%%    {Root, Tree}.
%%    HeadID = find_node_by_name("head", Tree),
%%    timer:sleep(3000),
%%    lists:foreach(
%%    fun(A) ->
%%	      timer:sleep(100),
%%	      _ = update_node(Tree, HeadID, [{a,[0, A,0]}])
%%      end, lists:seq(0, 90, 5)),
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
      is_number(A), is_number(Vx), is_number(Vy), is_number(Vz), ?is_children(Children) ->
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
    translate_(V,Params,Children).

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
	    Tree,IDGen,ParentID,Scope) ->
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

%%
%% update multiple nodes in the tree with new parameters
%% (currently only color and transformation nodes are updated)
%%
-spec update_nodes(Tree::tree(), [{node_id(),[node_param()]}]) -> tree().

update_nodes(Tree, List) ->
    {Tree1, UpdateMap} = update_nodes_(Tree, List, #{}),
    %% simple algortihm to update all children (to simple?)
    %% traverse the tree and update nodes and children to
    %% the nodes found in UpdateMap
    case update_tree(Tree1, UpdateMap) of
	{Tree2,[]} -> Tree2;
	{Tree2,UList} -> w_update(Tree2, UList)
    end.

update_nodes_(Tree, [{ID,Params}|List], UpdateMap) ->
    {Tree1,UpdateMap1} = update_node_(Tree, ID, Params, UpdateMap),
    update_nodes_(Tree1, List, UpdateMap1);
update_nodes_(Tree, [], UpdateMap) ->
    {Tree, UpdateMap}.

%%
%% update one node in the tree with new parameters
%% (currently only color and transformation nodes are updated)
%%
-spec update_node(Tree::tree(),ID::node_id(),
		  Params::[{Key::atom(), Value::term()}]) ->
	  tree().

update_node(Tree,ID,Params) 
  when is_map(Tree), is_integer(ID), is_list(Params) ->
    {Tree1,UpdateMap} = update_node_(Tree,ID,Params,#{}),
    Scope = maps:get(ID, UpdateMap, #{}),
    if map_size(Scope) =:= 0 ->
	    {Tree1,[]};
       true -> 
	    case update_sub_tree(Tree1,ID,Scope,UpdateMap,[]) of
		{Tree2,[]} -> Tree2;
		{Tree2,UList} -> 
		    w_update(Tree2, UList)
	    end
    end.


update_node_(Tree,ID,Params,UpdateMap) ->
    N = maps:get(ID, Tree),
    %% extract Name
    case N#node.type of
	node ->
	    %% do not (yet) update children
	    N1 = N#node{params=Params},
	    Tree1 = maps:put(ID, N1, Tree),
	    {Tree1, UpdateMap};
	scope ->
	    %% must update children when we allow object rebuild
	    %% include update $fn,$fs,$fa
	    N1 = N#node{params=Params},
	    Tree1 = maps:put(ID, N1, Tree),
	    {Tree1, UpdateMap};
	    %% Scope = #{ params => maps:from_list(Params) },
	    %% update_children(Tree1, N#node.children, Scope);
	color ->
	    Color1 = scad_eval:color(Params),
	    N1 = N#node { params = Params, color = Color1 },
	    Tree1 = maps:put(ID, N1, Tree),
	    {Tree1, add_update(ID, UpdateMap, color)};
	union ->
	    %% FIXME: do not (yet) update children
	    %% children should be collapsed! (all vertices collected!)
	    N1 = N#node{params=Params},
	    {maps:put(ID, N1, Tree), UpdateMap};
	difference ->
	    %% FIXME: do not (yet) update children
	    N1 = N#node{params=Params},
	    {maps:put(ID, N1, Tree), UpdateMap};
	intersection ->
	    %% FIXME: do not (yet) update children
	    N1 = N#node{params=Params},
	    {maps:put(ID, N1, Tree), UpdateMap};
	scale ->
	    Ltm = scale_ltm(proplists:get_value(v, Params, undef)),
	    set_ltm(Tree, N, ID, Params, Ltm, UpdateMap);
	translate ->
	    Ltm = translate_ltm(proplists:get_value(v, Params, undef)),
	    set_ltm(Tree, N, ID, Params, Ltm, UpdateMap);
	rotate ->
	    Ltm = rotate_ltm(proplists:get_value(a, Params, undef),
			     proplists:get_value(v, Params, undef)),
	    set_ltm(Tree, N, ID, Params, Ltm, UpdateMap)
	%% FIXME: update cube/sphere/cylinder this normally require
	%%        the object beeing rebuilt
    end.

%% set local transform matrix scale/translate/rotate nodes,
set_ltm(Tree, N, ID, Params, Ltm, UpdateMap) ->
    N1 = N#node { params = Params, ltm = Ltm },
    Tree1 = maps:put(ID, N1, Tree),
    {Tree1, add_update(ID, UpdateMap, ctm)}.

add_update(ID, UpdateMap, Key) ->
    Scope = maps:get(ID, UpdateMap, #{}),
    Scope1 = Scope#{ Key => true },
    UpdateMap#{ID => Scope1}.

%% used after update_node/s
update_tree(Tree, UpdateMap) ->
    RootID = find_root(Tree),
    update_sub_tree(Tree, RootID, #{}, UpdateMap, []).

update_sub_tree(Tree, ID, Scope, UpdateMap, UList) ->
    N = maps:get(ID, Tree),
    Scope1 = maps:get(ID, UpdateMap, #{}),
    Scope2 = maps:merge(Scope, Scope1),
    {Tree1,Scope3,UList1} = update_tree_node1_(Tree, N, Scope2, UList),
    update_branches_(Tree1, N#node.children, Scope3, UpdateMap, UList1).

update_branches_(Tree, [ID|IDs], Scope, UpdateMap, UList) ->
    {Tree1, UList1} = update_sub_tree(Tree, ID, Scope, UpdateMap, UList),
    update_branches_(Tree1, IDs, Scope, UpdateMap, UList1);
update_branches_(Tree, [], _Scope, _UpdateMap, UList) ->
    {Tree, UList}.

update_tree_node1_(Tree, N, Scope, UList) ->
    case maps:get(ctm, Scope, undefined) of
	undefined ->
	    update_tree_color_(Tree, N, Scope, UList);
	true -> %% update current ctm and pass to children
	    Ctm = case N#node.parent of
		      -1 ->  e3d_mat:identity();
		      PID -> (maps:get(PID, Tree))#node.ctm
		  end,
	    Ctm1  = e3d_mat:mul(Ctm, N#node.ltm),
	    update_tree_ctm_(Tree, N, Scope#{ctm=>Ctm1}, Ctm1, UList);
	Ctm ->
	    Ctm1  = e3d_mat:mul(Ctm, N#node.ltm),
	    update_tree_ctm_(Tree, N, Scope#{ctm=>Ctm1}, Ctm1, UList)
    end.

%% update color only
update_tree_color_(Tree, N, Scope, UList) ->
    case maps:get(color, Scope, undefined) of
	undefined ->
	    {Tree, Scope, UList};
	true ->
	    UList1 = [{N#node.id,N#node.color,undefined}|UList],
	    {Tree, Scope#{ color=>N#node.color}, UList1};
	Color ->
	    N1 = N#node { color = Color },
	    Tree1 = maps:put(N#node.id, N1, Tree),
	    UList1 = [{N#node.id,Color,undefined}|UList],
	    {Tree1, Scope, UList1}
    end.

%% update ctm & color
update_tree_ctm_(Tree, N, Scope, Ctm, UList) ->
    N1 = N#node { ctm = Ctm },
    case maps:get(color, Scope, undefined) of
	undefined -> %% no update
	    Tree1 = maps:put(N#node.id, N1, Tree),
	    {Tree1, Scope,
	     [{N#node.id,undefined,Ctm}|UList]};
	true ->  %% update current color, and pass to children
	    Tree1 = maps:put(N#node.id, N1, Tree),
	    {Tree1, Scope#{ color=> N#node.color},
	     [{N#node.id,N#node.color,Ctm}|UList]};
	Color -> %% set parent color from scope
	    N2 = N1#node { color = Color },
	    Tree1 = maps:put(N#node.id, N2, Tree),
	    {Tree1, Scope, [{N#node.id,Color,Ctm}|UList]}
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

%% update list of wings objects and verties
w_update(Tree, UList) ->
    %% io:format("UList=~p~n",[[ID||{ID,_,_} <- UList]]),
    WUL = lists:foldl(
	    fun({ID,Color,Ctm}, UL) ->
		    N = maps:get(ID, Tree),
		    case N#node.wid of
			-1 -> UL;
			Wid ->
			    [{Wid,Ctm,N#node.vs,Color}|UL]
		    end
	    end, [], UList),
    %% io:format("WUL=~p\n", [[Wid||{Wid,_,_,_}<- WUL]]),
    scad_wings:w_transform(WUL),
    Tree.

-spec find_node_by_name(Name::string(), Tree::tree()) -> node_id()|false.
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

%% find root of tree
%% by follwing the parent links to the root.
%% (FIXME: maybe store root => ID in the tree(map))
-spec find_root(Tree::tree()) -> node_id()|false.
find_root(Tree) ->
    Iter = maps:iterator(Tree),
    find_root_(Tree,Iter).
find_root_(Tree,Iter) ->
    case maps:next(Iter) of
	none ->
	    false;
	{ID,Node,_Iter1} ->
	    case Node#node.parent of
		-1 -> ID;
		ParentID ->
		    find_root__(Tree, ParentID)
	    end
    end.
find_root__(Tree, ID) ->
    Node = maps:get(ID, Tree),
    case Node#node.parent of
	-1 -> ID;
	ParentID ->
	    find_root__(Tree, ParentID)
    end.

%% find all roots of tree
%% by traversing all items in the map (very inefficient!)
-spec find_roots(Tree::tree()) -> [node_id()].
find_roots(Tree) ->
    Iter = maps:iterator(Tree),
    find_roots_(Iter, []).
find_roots_(Iter, Acc) ->
    case maps:next(Iter) of
	none -> Acc;
	{ID,Node,Iter1} ->
	    case Node#node.parent of
		-1 -> find_roots_(Iter1,[ID|Acc]);
		_ -> find_roots_(Iter1,Acc)
	    end
    end.
