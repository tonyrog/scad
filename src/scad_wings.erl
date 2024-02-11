%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Object creation and manipulation using wings3d as a backend
%%% @end
%%% Created : 22 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(scad_wings).
-compile(export_all).

-include("scad.hrl").
-include_lib("wx/include/wx.hrl").  %% Sigh need to fake event

-export([menu/2,command/2]).  %% plugin api

-export([demo/0]).
-export([demo_difference/0]).
-export([demo_union/0]).
-export([demo_car/0]).

-export([object_id_list/0]).
-export([get_object/1]).

%% objects
-export([render/1, render/2]).

%% 
-export([clear/0]). %% clear the current model
-export([select_all/0, deselect_all/0]).
-export([view_wireframe/0, view_shade/0]).
-export([select_mode_body/0, select_mode_face/0]).

%% selection
-export([selection_clear/0, 
	 selection_set/1, 
	 select_object/1, 
	 deselect_object/1,
	 selected_ids/0]).

%% internal tests
-export([r_cube/3, r_cylinder/3, r_sphere/3]).
-export([r_rotate/3, r_scale/3, r_translate/3, r_color/3]).
-export([r_union/2, r_intersection/2, r_difference/2]).
%% internal api
-export([r_cube_/3, r_cylinder_/3, r_sphere_/3]).

%% demo import
-include("scad_api.hrl").

%%low-level
-export([w_transform/2]).
-export([w_color/2]).
-export([fragments/4]).

%% -define(dbg(F,A), io:format(F,A)).
-define(dbg(F,A), ok).

%%-type color() :: {R::float(), G::float(), B::float()}.
-type vec3d() :: {X::float(), Y::float(), Z::float()}.
-type transform() :: e3d_mat:e3d_mat().

wings_pid() -> wings.
%% wings_pid() -> {wings, wings@localhost}.
ping(wings) -> pong;
ping(Node) when is_tuple(Node) -> net_adm:ping(Node).

demo() ->
    start_wings(),
    clear(),
    Tree = color(?RED,
		  [cube([1.0,1.0,1.0],[{center,true}]),
		   sphere(2.0),
		   cube([3.0,3.0,3.0],[{center,true}])]),
    render(Tree).

demo2() ->
    start_wings(),
    clear(),
    %% mRotate(45.0, [0.0,0.0,1.0],
    MeshColor = fun(X,Y) ->
			case (abs(X div 2)+abs(Y div 2)) rem 4 of
			    0 -> ?RED;
			    1 -> ?GREEN;
			    2 -> ?BLUE;
			    3 -> ?YELLOW
			end
		end,
    Tree = [ translate([X,Y,0.0],
		       [color(MeshColor(X,Y),
			      [sphere(1.0)])])
	     || X <- lists:seq(-10,10,2), Y <- lists:seq(-10,10,2)],
    All = lists:flatten(render(Tree)),
    TRef = erlang:start_timer(60000, self(), done),
    demo2_loop(5, All, All, TRef).

demo2_loop(Degree, [], All0, TRef) ->
    demo2_loop(Degree, All0, All0, TRef);
demo2_loop(Degree, All, All0, TRef) ->
    w_transform(e3d_mat:rotate(Degree, {0.0,0.0,1.0}), All),
    receive
	{timeout, TRef, done} ->
	    ok
    after 50 ->
	    demo2_loop(Degree, tl(All), All0, TRef)
    end.

demo_car() ->
    start_wings(),
    clear(),
    Tree = car:car(),
    render(Tree).    

demo_difference() ->
    start_wings(),
    clear(),
    Tree = color(?RED,
		  [difference([cube([1.0,1.0,3.2],[{center,true}]),
			       sphere(2.0)])]),
    render(Tree).


demo_union() ->
    start_wings(),
    clear(),
    Tree = union(
	     [
	      cylinder(4.0,1.0,[{center,true},{'$fn',100}]),
	      rotate(90,[1.0,0.0,0.0],
		     [cylinder(4.0,0.9,[{center,true},{'$fn',100}])])
	      ]),
    render(Tree).

demo_bool() ->
    start_wings(),
    clear(),
    Tree = 
	[
	 translate([15.0,15.0,0.0], 
		   union([cube(6, [{center,true}]), sphere(8)])),
	 translate([15.0,-15.0,0.0], 
		   intersection([cube(6,[{center,true}]), sphere(8)])),
	 translate([-15.0,15.0,0.0], 
		   difference([cube(6,[{center,true}]), sphere(8)])),
	 translate([-15.0,-15.0,0.0], 
		   difference([sphere(8),cube(6,[{center,true}])]))
	],
    render(Tree).


start_wings() ->
    case whereis(wings) of
	undefined ->
	    wings:start(),
	    external_sync(30000);
	_Pid when is_pid(_Pid) ->
	    ok
    end,
    init_commands(),
    external_sync().


dirty() ->
    wings_pid() ! {timeout, make_ref(), {event, {wm,dirty}}}.


windows() ->
    wm_callback(fun() ->
			gb_trees:keys(get(wm_windows))
		end).

plugins() ->
    wm_callback(fun() ->
			wings_wm:get_value(wings_plugins)
		end).

add_plugin(Mod) when is_atom(Mod) ->
    wm_callback(fun() ->
			Ps = wings_wm:get_value(wings_plugins),
			case lists:member(Mod, Ps) of
			    true -> ok;
			    false ->
				wings_wm:set_value(wings_plugins, [Mod|Ps])
			end
		end).

remove_plugin(Mod) when is_atom(Mod) ->
    wm_callback(fun() ->
			Ps = wings_wm:get_value(wings_plugins),
			wings_wm:set_value(wings_plugins,
					   lists:delete(Mod, Ps))
		end).

objects() ->
    wm_callback(fun() ->
			gb_trees:keys(get(wm_windows))
		end).


get_object(Id) when is_integer(Id) -> plugin_call({get_object,[Id]}).
object_id_list() -> plugin_call({object_id_list,[]}).

selection_clear() ->
    plugin_call({selection_clear,[]}).
selection_set(IDs) when is_list(IDs), (IDs=:=[] orelse is_integer(hd(IDs))) ->
    plugin_call({selection_set,IDs}).
select_object(Id) when is_integer(Id) ->
    plugin_call({select_object,[Id]}).
deselect_object(Id) when is_integer(Id) ->
    plugin_call({deselect_object,[Id]}).

selected_ids() ->
    plugin_call({selected_ids,[]}).

-spec w_transform(
	[{ID::integer(),Matrix::transform(),Vs::[vec3d()],Color::color()}]) ->
	  ok.

w_transform(List) ->
    plugin_call({transform,[List]}).

-spec w_transform(Matrix::transform(), IDs::[integer()]) -> ok.

w_transform(Matrix, IDs) ->
    plugin_call({transform,[Matrix,IDs]}).

-spec w_color(Color::color(), IDs::[integer()]) -> ok.
w_color(Color, IDs) ->
    plugin_call({set_color,[Color,IDs]}).
    
clear() ->
    wings_action({file, confirmed_new}, true).

select_all() ->
    wings_action({select, all}, true).

deselect_all() ->
    selection_clear().

view_wireframe() ->
    deselect_all(),
    wings_action({view, wireframe}, true).

view_shade() ->
    deselect_all(),
    wings_action({view, shade}, true).

select_mode_body() ->
    wings_action({select, body}, true).

select_mode_face() ->
    wings_action({select, face}, true).

quit() ->
    quit(false).
quit(Exit) when is_integer(Exit); Exit =:= false ->
    verbose("Script: exiting\n", []),
    Ref = monitor(process, wings),
    action_cast({file, confirmed_quit}),
    if Exit =:= false ->
	    ok;
       true ->
	    receive
		{'DOWN',Ref,_,_,_Reason} ->
		    timer:sleep(100),
		    halt(Exit)
	    after 1000 ->
		    halt(Exit)
	    end
    end.

import_file(Filename,wings) ->
    call_wings({file, {confirmed_open, Filename}});
import_file(Filename,nendo) ->
    call_wings({file, {import, {ndo, Filename}}});
import_file(Filename,Type) ->
    action_cast(fun() -> put(wings_not_running, {import, Filename}), keep end),
    call_wings({file, {import, Type}}).
    
export_file(Type, FileName) ->
    call_wings({file, {export, {Type, FileName}}}).

save_as(Type, FileName) ->
    call_wings({file, {save_as, {Type, FileName}}}).

call_wings(Cmd) ->
    verbose("Script: ~p~n",[{action, Cmd}]),
    action_cast(Cmd),
    external_sync().

wm_callback(Fun) ->
    SELF = self(),
    REF =  make_ref(),
    wings_pid() ! {wm,{callback,fun() ->
				  SELF ! {REF,Fun()}
			  end}},
    receive
	{REF,Result} ->
	    Result
    end.

verbose(F,A) ->
    case get(verbose) of
	undefined -> false;
	Flag -> Flag
    end andalso io:format(F,A).

render(Objs) ->
    render(Objs, #{}).

render([Obj|Objs], Scope) -> 
    lists:flatten([render(Obj, Scope) | render(Objs, Scope)]);
render([], _Scope) -> [];
render(#object{type=Type, params=Params, children=Children}, Scope) ->
    case Type of
	scope -> 
	    Scope1 = maps:merge(Scope, maps:from_list(Params)),
	    render(Children, Scope1);
	cube -> r_cube(Params, Children, Scope);
	cylinder -> r_cylinder(Params, Children, Scope);
	sphere -> r_sphere(Params, Children, Scope);
	intersection -> r_intersection(Children, Scope);
	difference -> r_difference(Children, Scope);
	union -> r_union(Children, Scope);
	rotate -> r_rotate(Params, Children, Scope);
	scale -> r_scale(Params, Children, Scope);
	translate -> r_translate(Params, Children, Scope);
	color -> r_color(Params, Children, Scope)
    end.

children(Child, Scope) when is_record(Child, object) -> 
    render([Child], Scope);
children(Children, Scope) when is_list(Children) ->
    render(Children, Scope).

r_rotate(Params, Children, Scope) ->
    r_rotate_(Params, children(Children, Scope), Scope).

r_rotate_(Params, Objs, _Scope) ->
    V = case proplists:get_value(v, Params) of
	    undefined -> {1.0, 0.0, 0.0};
	    [X,Y,Z] -> {X,Y,Z}
	end,
    Matrix = case proplists:get_value(a, Params) of
		 undefined ->
		     e3d_mat:rotate_from_euler_rad(V);
		 A ->
		     e3d_mat:rotate(A, V)
	     end,
    w_transform(Matrix, Objs).

r_scale(Params, Children, Scope) ->
    r_scale_(Params, children(Children, Scope), Scope).

r_scale_(Params, Objs, _Scope) ->
    V = case proplists:get_value(v, Params) of
	    undefined -> {1.0, 1.0, 1.0};
	    [X,Y,Z] -> {X,Y,Z}
	end,
    Matrix = e3d_mat:scale(V),
    w_transform(Matrix, Objs).

r_translate(Params, Children, Scope) ->
    r_translate_(Params, children(Children, Scope), Scope).

r_translate_(Params, Objs, _Scope) ->
    Vec = case proplists:get_value(v, Params) of
	      undefined -> {0.0,0.0,0.0};
	      [X,Y,Z] -> {X,Y,Z}
	  end,
    Matrix = e3d_mat:translate(Vec),
    w_transform(Matrix, Objs).


r_union(Children,Scope) ->
    r_union_(children(Children, Scope),Scope).
r_union_([A],_Scope) ->
    [A];
r_union_([A,B|Objs],Scope) ->
    C = plugin_call({union,[A,B]}),
    r_union_([C|Objs],Scope).

r_intersection(Children, Scope) ->
    r_intersection_(children(Children, Scope), Scope).
r_intersection_([A], _Scope) ->
    [A];
r_intersection_([A,B|Objs], Scope) ->
    C = plugin_call({intersection,[A,B]}),
    r_intersection_([C|Objs], Scope).

r_difference(Children, Scope) ->
    Win = wings_exec(fun() -> get(gl_canvas) end),
    io:format("Win = ~p\n", [Win]),
    r_difference_(children(Children, Scope), Win, Scope).
r_difference_([A], _Win, _Scope) -> 
    io:format("difference: return = ~p~n",[A]),
    [A];
r_difference_([A,B|Objs],Win,Scope) ->
    io:format("difference: ~p - ~p~n",[A,B]),
    _Ref = plugin_cast({difference,[A]}),
    Cmd = {select, {by, {id, {"dummy prompt", [{B,gb_sets:singleton(0)}]}}}},
    action_cast(Cmd),
    execute_on_r(Win),
    continue(100),
    r_difference_([A|Objs],Win,Scope).

continue(Time) ->
    timer:sleep(Time),
    wings_pid() ! {external, fun(_) -> keep end}.

execute_on_r(Win) ->
    %% Pop the event handler with right mouse up
    MouseEvent = #wxMouse{ type=right_up,
			   x=5, y=5,
			   leftDown = false, 
			   middleDown = false,
			   rightDown = false,
			   controlDown = false,
			   shiftDown = false,
			   altDown = false,
			   metaDown = false},
    Event = #wx{obj=Win,event=MouseEvent},
    wings_pid() ! Event.



r_color(Params, Children, Scope) ->
    r_color_(Params, children(Children, Scope), Scope).
r_color_(Params, Objs, _Scope) ->
    Alpha = proplists:get_value(alpha, Params, undef),
    Color = case proplists:get_value(c, Params) of
		[R,G,B] when Alpha =:= undef -> {R,G,B};
		[R,G,B] -> {R,G,B,Alpha};
		[R,G,B,A] when Alpha =:= undef -> {R,G,B,A};
		[R,G,B,_A] -> {R,G,B,Alpha}
	    end,
    w_color(Color, Objs).


r_cube(Params, Children, Scope) ->
    {ID,_Vs} = r_cube_(Params, children(Children,Scope), Scope),
    ID.

r_cube_(Params, _Objs, Scope) ->
    [X,Y,Z] = case proplists:get_value(size, Params) of
		  undefined -> [1.0,1.0,1.0];
		  Vec -> Vec
	      end,
    [Dx,Dy,Dz] = case proplists:get_value(center, Params) of
		     false -> [X/2,Y/2,Z/2];
		     true -> [0.0,0.0,0.0]
		 end,
    OptList = [{nres, 1},
	       {xcube, X}, {ycube, Y}, {zcube, Z},
	       {rot_x,0.0},{rot_y,0.0},{rot_z,0.0},
	       {mov_x,Dx}, {mov_y,Dy}, {mov_z,Dz},
	       {ground, false},
	       {spherizeflag,false}
	      ],
    Ctm = maps:get(ctm,Scope, identity),
    Color  = maps:get(color,Scope, ?GOLD),
    plugin_call({cube,[Ctm,Color,OptList]}).

%% ["h","r","r1","r2","center","d","d1","d2"],
r_cylinder(Params, Children, Scope) ->
    {ID,_Vs} = r_cylinder_(Params, children(Children, Scope), Scope),
    ID.

r_cylinder_(Params, _Objs, Scope) ->
    H = proplists:get_value(h, Params, 1.0),
    D = proplists:get_value(d, Params, undef),
    D1 = proplists:get_value(d1, Params, undef),
    D2 = proplists:get_value(d2, Params, undef),
    R  = proplists:get_value(r, Params, undef),
    R1 = proplists:get_value(r1, Params, undef),
    R2 = proplists:get_value(r2, Params, undef),

    RR1 = if D1 == undef, D /= undef -> D/2;
	     D1 == undef -> if R1 == undef -> R; true -> R1 end;
	     true -> D1/2
	  end,
    RR2 = if D2 == undef, D /= undef -> D/2;
	     D2 == undef -> if R2 == undef -> R; true -> R2 end;
	     true -> D2/2
	  end,
    RR = max(RR1, RR2),
    [Dx,Dy,Dz] = case proplists:get_value(center, Params, false) of
		     false -> [RR,RR,H/2];
		     true -> [0.0,0.0,0.0]
		 end,
    Fa = proplists:get_value('$fa', Params, maps:get('$fa',Scope,12)),
    Fn = proplists:get_value('$fn', Params, maps:get('$fn',Scope,0)),
    Fs = proplists:get_value('$fs', Params, maps:get('$fs',Scope,2)),
    Ns = fragments(RR, Fn, Fs, Fa),
    Thickness = 1, %% scad parameter? (cylinder_type = tube|gear)
    OptList = [{cylinder_type,cylinder}, 
	       {sections,Ns},
	       {height,H},
	       {thickness, Thickness},
	       {rot_x,0},{rot_y,0},{rot_z,0},
	       {mov_x,Dx}, {mov_y,Dy}, {mov_z,Dz},
	       {top_x, RR1}, {top_z, RR1}, {bottom_x, RR2}, {bottom_z, RR2},
	       {ground, false}],
    Ctm = maps:get(ctm,Scope, identity),
    Color  = maps:get(color,Scope, ?GOLD),
    plugin_call({cylinder,[Ctm,Color,OptList]}).

r_sphere(Params, Children, Scope) ->
    {ID,_Vs} = r_sphere_(Params, children(Children, Scope), Scope),
    ID.

r_sphere_(Params, _Objs, Scope) ->
    R = case proplists:get_value(d, Params) of
	    undefined ->
		case proplists:get_value(r, Params) of
		    undefined -> 1.0;
		    R0 -> R0
		end;
	    D -> D/2
	end,
    %% default: $fn=0, $fa=12, $fs=2
    %% $fa = fragment angle in degrees
    %% $fs = fragment size in mm
    %% $fn = resolution
    %%
    %% Ns = number of sections
    %% Nl = number of slices
    %% default parameters should be sent in a envirnment
    Fn = proplists:get_value('$fn', Params, maps:get('$fn',Scope,0)),
    Fa = proplists:get_value('$fa', Params, maps:get('$fa',Scope,6)), %% 12),
    Fs = proplists:get_value('$fs', Params, maps:get('$fs',Scope,0.5)), %%2),
    Ns = fragments(R, Fn, Fs, Fa),
    Nl = max(Ns div 2, 3),
    Xr = R,
    Yr = R,
    OptList = [{rot_x,0.0},{rot_y,0.0},{rot_z,0.0},
	       {mov_x,0.0}, {mov_y,0.0}, {mov_z,0.0},
	       {ground, false}],
    Ctm = maps:get(ctm,Scope, identity),
    Color  = maps:get(color,Scope, ?GOLD),
    plugin_call({sphere,[Ctm,Color,[Ns,Nl,Xr,Yr | OptList]]}).

-define(GRID_FINE, 0.01).

fragments(R, _Fn, _Fs, _Fa) when R < ?GRID_FINE -> 3;
fragments(_R, Fn, _Fs, _Fa) when Fn > 0.0 ->
    if Fn >= 3 -> trunc(Fn);
       true -> 3
    end;
fragments(R, _Fn, Fs, Fa) ->
    trunc(math:ceil(max(min(360.0/Fa, R*2*math:pi()/Fs), 5))).

%%
%% Call wings by installing a special command plugin 
%% to wrap the calls and using internal api's
%%

%% local plugin callbacks
init_commands() ->
    add_plugin(?MODULE).


external_call(Call) ->
    Ref = make_ref(),   %% fixme: handle wings crash? monitor
    XCall = {call,Call,{self(),Ref}},
    wings_pid() ! {external, fun(St) -> command(XCall, St) end},
    receive
	{Ref,{exception,Error}} -> error(Error);
	{Ref,Result} -> Result
    end.


wings_action(Cmd, Sync) ->
    action_cast(Cmd),
    case Sync of
        false -> ok;
        true ->  wings_exec(fun() -> sync end)
    end.

action_cast(Cast) ->
    wings_pid() ! {action, Cast}.


wings_exec(Fun) ->
    wings_exec(Fun,2000).
wings_exec(Fun,Timeout) when is_function(Fun) ->
    Caller = self(),
    Ref = make_ref(),
    Do = fun(_St) ->
                 Reply = Fun(),
                 Caller ! {Ref, Reply},
                 keep
         end,
    wings_pid() ! {external, Do},
    receive
	{Ref, Reply} -> Reply
    after Timeout -> timeout
    end.

%% action call is used with init_commands to install local plugin
plugin_call(Call) ->
    Ref = plugin_call_(Call),
    wait_reply_(Ref).

plugin_call_(Call) ->
    Ref = make_ref(),   %% fixme: handle wings crash? monitor
    wings_pid() ! {action,{call,Call,{self(),Ref}}},
    Ref.

plugin_cast(Cast) ->
    Ref = make_ref(),   %% fixme: handle wings crash? monitor
    wings_pid() ! {action,{cast,Cast,{self(),Ref}}},
    Ref.

wait_reply_(Ref) -> wait_reply_(Ref, infinity).
wait_reply_(Ref, Timout) ->
    receive
	{Ref,{exception,Error}} -> error(Error);
	{Ref,Result} -> Result
    after Timout ->
	    error({timeout, Timout})
    end.
    
%% FIXME: this is a hack to get the wings shapes and we defs
-include("mini_wings.hrl").

shapes(St) ->
    St#st.shapes.
%% element(2, St).

get_latest_id_(St) ->
    Shapes = shapes(St),
    case gb_trees:size(Shapes) of
	0 -> {0,nil};
	_ -> gb_trees:largest(Shapes)
    end.

external_sync() ->
    external_sync(5000).
external_sync(Timeout) ->
    SELF = self(),
    REF =  make_ref(),
    Sync = fun(_St) ->
                   SELF ! {REF,sync},
                   keep
           end,
    wings_pid() ! {external, Sync},
    receive 
	{REF, sync} -> ok 
    after
	Timeout ->
	    timeout
    end.

reply({Caller,Ref}, Result) ->
    Caller ! {Ref, Result}.

menu(_, Menu) -> Menu.

command(Call, St) ->
    ?dbg("plugin called: ~p\n", [Call]),
    command_(Call, St).

command_({call,{get_object,[Id]},From}, St) ->
    try wings_obj:get(Id, St) of
	Obj when is_map(Obj) ->
	    reply(From, Obj)
    catch
	error:_ ->
	    reply(From, {exception,{error, {no_object, Id}}})
    end,
    keep;
command_({call,{object_id_list,[]},From}, St) ->
    St1 = wings_sel:clear(St), %% trick to get all object ids.
    IDs = wings_sel:unselected_ids(St1), 
    reply(From, IDs),
    keep;
%% selection (wrap wings_sel?
command_({call,{selection_clear,[]},From}, St) ->
    St1 = wings_sel:clear(St),
    reply(From, ok),
    St1;
command_({call,{selection_set,IDs},From}, St) ->
    Sel = [{ID,gb_sets:singleton(0)} || ID <- IDs],
    St1 = wings_sel:set(body, Sel, St),
    reply(From, ok),
    St1;
command_({call,{select_object,[Id]},From}, St) ->
    St1 = wings_sel:select_object(Id, St),
    reply(From, ok),
    St1;
command_({call,{deselect_object,[Id]},From}, St) ->
    St1 = wings_sel:deselect_object(Id, St),
    reply(From, ok),
    St1;
command_({call,{selected_ids,[]},From}, St) ->
    reply(From, wings_sel:selected_ids(St)),
    keep;
command_({call,{cube,[Ctm,Color,Arg]},From}, St) ->
    try wpc_ncube:command({shape,{ncube, Arg}}, St) of
	{new_shape,Name,Fs,Vs} ->
	    ?dbg("new_shape: ~p ~p ~p\n", [Name,Fs,Vs]),
	    We = build(Fs, Vs, Ctm, Color),
	    St1 = wings_obj:new(Name, We, St),
	    {ID,_} = get_latest_id_(St1),
	    reply(From, {ID,Vs}),
	    St1
    catch
	error:_:Stack ->
	    io:format("crash: ~p~n", [Stack]),
	    reply(From, {exception,{badarg,{cube, Arg}}}),
	    keep
    end;
command_({call,{cylinder,[Ctm,Color,Arg]},From}, St) ->
    try wpc_cylinder:command({shape,{cylinder, Arg}}, St) of
	{new_shape,Name,Fs,Vs} ->
	    ?dbg("new_shape: ~p ~p ~p\n", [Name,Fs,Vs]),
	    We = build(Fs, Vs, Ctm, Color),
	    St1 = wings_obj:new(Name, We, St),
	    {ID,_} = get_latest_id_(St1),
	    reply(From, {ID,Vs}),
	    St1
    catch
	error:_:Stack ->
	    io:format("crash: ~p~n", [Stack]),
	    reply(From, {exception,{badarg,{cylinder, Arg}}}),
	    keep
    end;
command_({call,{sphere,[Ctm,Color,Arg]},From}, St) ->
    try make_sphere(Arg, St) of
	{new_shape,Name,Fs,Vs} ->
	    ?dbg("new_shape: ~p ~p ~p\n", [Name,Fs,Vs]),
	    We = build(Fs, Vs, Ctm, Color),
	    St1 = wings_obj:new(Name, We, St),
	    {ID,_} = get_latest_id_(St1),
	    reply(From, {ID,Vs}),
	    St1
    catch
	error:_:Stack ->
	    io:format("crash: ~p~n", [Stack]),
	    reply(From, {exception,{badarg,{sphere, Arg}}}),
	    keep
    end;
command_({call,{set_color,[Color,Objs]},From}, St) ->
    ColorRGB = rgb(Color),
    try wings_obj:update(
	  fun(We) ->
		  wings_va:set_body_color(ColorRGB, We)
	  end, Objs, St) of
	St1 ->
	    reply(From, Objs),
	    St1
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{color,[Color,Objs]}}}),
	    keep
    end;
command_({call,{transform,[List]},From}, St) ->
    try lists:foldl(
	  fun(E={ID,Ctm,Vs,Color}, St1) ->
		  io:format("transform: ~p~n", [E]),
		  transform_vs(ID, Ctm, Color, Vs, St1)
	  end, St, List) of
	St1 ->
	    reply(From, ok),
	    St1
    catch
	error:_:Stack ->
	    io:format("crash: ~p~n", [Stack]),
	    reply(From, {exception,{badarg,{transform,[List]}}}),
	    keep
    end;
command_({call,{transform,[Ctm,Objs]},From}, St) ->
    try wings_obj:update(
	  fun(We) ->
		  wings_we:transform_vs(Ctm, We)
	  end, Objs, St) of
	St1 ->
	    reply(From, Objs),
	    St1
    catch
	error:_:Stack ->
	    io:format("crash: ~p~n", [Stack]),
	    reply(From, {exception,{badarg,{transform,[Ctm,Objs]}}}),
	    keep
    end;
%% since difference/sub need prompt it is a bit more complex
command_({cast,{difference,[A]},_From}, St) ->
    Sel = [{A,gb_sets:singleton(0)}],
    St1 = wings_sel:set(body, Sel, St),
    wings_body:command({bool, sub}, St1);

command_({call,{intersection,Objs},From}, St) ->
    %% [A,B,C,D] => intersection = ((A * B) * C) * D
    bool_command(isect,intersection,Objs,From,St);

command_({call,{difference,Objs},From}, St) ->
    %% NOT WORKING !!!
    %% [A,B,C,D] => difference = ((A - B) - C) - D
    bool_command(sub,difference, Objs,From,St);
command_({call,{union,Objs},From}, St) ->
    %% [A,B,C,D] => union = ((A + B) + C) + D
    bool_command(add,union,Objs,From,St);
command_({call,Call,From}, _St) ->
    reply(From, {error, {unknown_command, Call}}),
    next;
command_(_, _St) ->
    next.

bool_command(Bool,Cmd,Objs,From,St) ->
    Sel = [{ID,gb_sets:singleton(0)} || ID <- Objs], %% select all objs
    St1 = wings_sel:set(body, Sel, St),              %% assign it
    try wings_body:command({bool, Bool}, St1) of
	{save_state, St2} ->
	    {ID,_} = get_latest_id_(St2),
	    reply(From, ID),
	    St2
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{Cmd,Objs}}}),
	    keep
    end.

%%
%% build
%%
build(Fs, Vs, Ctm, Color) ->
    %% Vs1 = [e3d_mat:mul_point(Matrix, V) || V <- Vs].
    We = wings_we:build(Fs, Vs),
    We1 = if Ctm =:= identity ->
		  We;
	     true ->
		  wings_we:transform_vs(Ctm, We)
	  end,
    wings_va:set_body_color(rgb(Color), We1).

%% set new (stored) Vs after transforming with matrix
transform_vs(ID, Ctm, Color, Vs, St) when is_list(Vs) ->
    ColorRGB = rgb(Color),
    PosTup = list_to_tuple(Vs),
    wings_obj:update(
      fun(We) ->
	      %% io:format("vp1: ~p~n", [We#we.vp]),
	      We1 = transform_vs_1(We, Ctm, PosTup),
	      %% io:format("vp2: ~p~n", [We1#we.vp]),
	      We2 = wings_va:set_body_color(ColorRGB, We1),
	      We2
      end, [ID], St).


transform_vs_1(We, Ctm, PosTup) ->
    Vtab = array:sparse_foldl(
	     fun(V,_Pos,A) ->
		     %% io:format("V = ~p\n,", [V]),
		     Pos = element(V+1, PosTup),
		     [{V,e3d_mat:mul_point(Ctm, Pos)}|A]
	     end, [], We#we.vp),
    We#we{vp=array:from_orddict(lists:reverse(Vtab))}.


%%
%% Copied sphere creation from wings_shapes
%%

make_sphere([Ns,Nl,Xr,Yr|Transf], _St) ->
    Xi = Xr/2.0,
    Yi = Yr/2.0,
    Fs = sphere_faces(Ns, Nl),
    Vs0 = sphere_circles(Ns, Nl, Xi, Yi) ++ [{0.0, Xi, 0.0}, {0.0, -Xi, 0.0}],
    Vs = wings_shapes:transform_obj(Transf, Vs0),
    %% The string below is intentionally not translated.
    {new_shape, "sphere", Fs, Vs}.

sphere_faces(Ns, Nl) ->
    Lasti= (Nl-1)*Ns,
    Topi= Lasti,
    Boti= Topi+1,
    Topf= [[Topi, (I+1) rem Ns, I]
	   || I <- lists:seq(0, Ns-1)],
    Botf= [[Boti, Lasti - Ns + I, Lasti - Ns + (I+1) rem Ns]
	   || I <- lists:seq(0, Ns-1)],
    Slices= [ [ [(I+1) rem Ns  +J*Ns,
		 (I+1) rem Ns  +J*Ns +Ns,
		 I             +J*Ns +Ns,
		 I             +J*Ns]
		|| I <- lists:seq(0, Ns-1)]
	      || J <- lists:seq(0, Nl-3)],
    Topf ++ Botf ++ lists:append(Slices).

sphere_circles(Ns, Nl, Xi, Yi) ->
    Delta = math:pi() / Nl,
    PosAndRads= [{math:cos(I*Delta), math:sin(I*Delta)} ||
		    I <- lists:seq(1, Nl-1)],
    Circles = [circle(Ns, Pos*Xi, Rad*Yi) || {Pos, Rad} <- PosAndRads],
    lists:flatten(Circles).

circle(N, Y, R) ->
    Delta = math:pi()*2 / N,
    [{R*math:cos(I*Delta), Y, R*math:sin(I*Delta)} || I <- lists:seq(0, N-1)].


rgb([R,G,B|_]) -> {R,G,B}.
