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

-export([object_id_list/0]).
-export([get_object/1]).
%% selection
-export([selection_clear/0, 
	 selection_set/1, 
	 select_object/1, 
	 deselect_object/1,
	 selected_ids/0]).
%% objects
-export([render/1]).
-export([cube/1, 
	 cylinder/1, 
	 sphere/1]).
%% transform
-export([rotate/2, 
	 scale/2, 
	 translate/2, 
	 color/2]).
%%low-level
-export([w_transform/2]).
-export([w_color/2]).
-export([fragments/4]).

%% -define(dbg(F,A), io:format(F,A)).
-define(dbg(F,A), ok).

wings_pid() -> wings.
%% wings_pid() -> {wings, wings@localhost}.
ping(wings) -> pong;
ping(Node) when is_tuple(Node) -> net_adm:ping(Node).


-define(RED, [1.0, 0.0, 0.0]).
-define(GREEN, [0.0, 1.0, 0.0]).
-define(BLUE, [0.0, 0.0, 1.0]).
-define(YELLOW, [1.0, 1.0, 0.0]).

mCube(X,Params) when is_number(X) ->
    #object{type=cube,params=[{size,[X,X,X]}|Params]};
mCube(Vec,Params) when is_list(Vec) ->
    #object{type=cube,params=[{size,Vec}|Params]}.
mCube(Vec) -> mCube(Vec,[]).

mSphere(R,Params) when is_number(R) -> 
    #object{type=sphere,params=[{r,R}|Params]}.
mSphere(R) when is_number(R) ->  mSphere(R,[]);
mSphere(Params) when is_list(Params) -> mSphere(1.0,Params).

mCylinder(H,R1,R2,Params) when is_number(H),is_number(R1), is_number(R2),
			       is_list(Params) ->
    #object{type=cylinder,params=[{h,H},{r1,R1},{r2,R2}|Params]}.
mCylinder(H,R,Params) -> mCylinder(H,R,R,Params).
mCylinder(H,R) -> mCylinder(H,R,R,[]).
mCone(H,R,Params) -> mCylinder(H,0.0,R,Params).
mCone(H,R) -> mCylinder(H,0.0,R,[]).


mIntersection(Children) ->
    #object{type=intersection,children=Children}.
mDifference(Children) ->
    #object{type=difference,children=Children}.
mUnion(Children) ->
    #object{type=union,children=Children}.

mColor(Color,Children) ->
    #object{type=color,params=[{c,(Color)}],children=Children}.
mRotate(Angle,Vec,Children) ->
    #object{type=rotate,params=[{a,(Angle)},{v,(Vec)}],children=(Children)}.
mTranslate(Vec,Children) ->
    #object{type=translate,params=[{v,(Vec)}],children=(Children)}.
mScale(Vec,Children) ->
    #object{type=scale,params=[{v,(Vec)}],children=(Children)}.

demo() ->
    start_wings(),
    Tree = mColor(?RED,
		  [mCube([1.0,1.0,1.0],[{center,true}]),
		   mSphere(2.0),
		   mCube([3.0,3.0,3.0],[{center,true}])]),
    render(Tree).

demo2() ->
    start_wings(),
    %% mRotate(45.0, [0.0,0.0,1.0],
    MeshColor = fun(X,Y) ->
			case (abs(X div 2)+abs(Y div 2)) rem 4 of
			    0 -> ?RED;
			    1 -> ?GREEN;
			    2 -> ?BLUE;
			    3 -> ?YELLOW
			end
		end,
    Tree = [ mTranslate([X,Y,0.0],
			[mColor(MeshColor(X,Y),
				[mSphere(1.0)])])
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

demo_difference() ->
    start_wings(),
    Tree = mColor(?RED,
		  [mDifference([mCube([1.0,1.0,3.2],[{center,true}]),
				mSphere(2.0)])]),
    render(Tree).


demo_union() ->
    start_wings(),
    Tree = mUnion(
	     [
	      mCylinder(4.0,1.0,[{center,true},{'$fn',100}]),
	      mRotate(90,[1.0,0.0,0.0],
		      [mCylinder(4.0,0.9,[{center,true},{'$fn',100}])])
	      ]),
    render(Tree).

demo_bool() ->
    start_wings(),
    Tree = 
	[
	 mTranslate([15.0,15.0,0.0], 
		    mUnion([mCube(6, [{center,true}]), mSphere(8)])),
	 mTranslate([15.0,-15.0,0.0], 
		    mIntersection([mCube(6,[{center,true}]), mSphere(8)])),
	 mTranslate([-15.0,15.0,0.0], 
		    mDifference([mCube(6,[{center,true}]), mSphere(8)])),
	 mTranslate([-15.0,-15.0,0.0], 
		    mDifference([mSphere(8),mCube(6,[{center,true}])]))
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

call_wings(Cmd) ->
    verbose("Script: ~p~n",[{action, Cmd}]),
    action_cast(Cmd),
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

-spec w_transform(Matrix::e3d_mat:e3d_mat(), IDs::[integer()]) -> ok.

w_transform(Matrix, IDs) ->
    plugin_call({transform,[Matrix,IDs]}).

-spec w_color(Color::{R::float(), G::float(), B::float()}, 
		IDs::[integer()]) -> ok.
w_color(Color={R,G,B}, IDs) when is_float(R), is_float(G), is_float(B) ->
    plugin_call({set_color,[Color,IDs]}).
    
quit(Exit) ->
    verbose("Script: exiting\n", []),
    Ref = monitor(process, wings),
    action_cast({file, confirmed_quit}),
    receive
        {'DOWN',Ref,_,_,_Reason} ->
            timer:sleep(100),
            halt(Exit)
    after 1000 ->
            halt(Exit)
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


render([Obj|Objs]) -> 
    lists:flatten([render(Obj) | render(Objs)]);
render([]) -> [];
render(#object{type=Type, params=Params, children=Children}) ->
    case Type of
	cube -> cube(Params);
	cylinder -> cylinder(Params);
	sphere -> sphere(Params);
	intersection -> intersection(Children);
	difference -> difference(Children);
	union -> union(Children);
	rotate -> rotate(Params, Children);
	scale -> scale(Params, Children);
	translate -> translate(Params, Children);
	color -> color(Params, Children)
    end.

children(Child) when is_record(Child, object) -> 
    render([Child]);
children(Children) when is_list(Children) ->
    render(Children).

rotate(Params, Children) ->
    rotate_(Params, children(Children)).

rotate_(Params, Objs) ->
    Angle = proplists:get_value(a, Params),
    V = case proplists:get_value(v, Params) of
	    undefined -> {1.0, 0.0, 0.0};
	    [X,Y,Z] -> {X,Y,Z}
	end,
    Matrix = e3d_mat:rotate(Angle, V),
    w_transform(Matrix, Objs).

scale(Params, Children) ->
    scale_(Params, children(Children)).

scale_(Params, Objs) ->
    V = case proplists:get_value(v, Params) of
	    undefined -> {1.0, 1.0, 1.0};
	    [X,Y,Z] -> {X,Y,Z}
	end,
    Matrix = e3d_mat:scale(V),
    w_transform(Matrix, Objs).

translate(Params, Children) ->
    translate_(Params, children(Children)).

translate_(Params, Objs) ->
    Vec = case proplists:get_value(v, Params) of
	      undefined -> {0.0,0.0,0.0};
	      [X,Y,Z] -> {X,Y,Z}
	  end,
    Matrix = e3d_mat:translate(Vec),
    w_transform(Matrix, Objs).

intersection(Children) ->
    intersection_(children(Children)).
intersection_([A]) ->
    [A];
intersection_([A,B|Objs]) ->
    C = plugin_call({intersection,[A,B]}),
    intersection_([C|Objs]).
    

difference(Children) ->
    Win = wings_exec(fun() -> get(gl_canvas) end),
    io:format("Win = ~p\n", [Win]),
    difference_(children(Children), Win).
difference_([A], _Win) -> 
    io:format("difference: return = ~p~n",[A]),
    [A];
difference_([A,B|Objs],Win) ->
    io:format("difference: ~p - ~p~n",[A,B]),
    _Ref = plugin_cast({difference,[A]}),
    Cmd = {select, {by, {id, {"dummy prompt", [{B,gb_sets:singleton(0)}]}}}},
    action_cast(Cmd),
    execute_on_r(Win),
    continue(100),
    difference_([A|Objs],Win).

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


union(Children) ->
    union_(children(Children)).
union_([A]) ->
    [A];
union_([A,B|Objs]) ->
    C = plugin_call({union,[A,B]}),
    union_([C|Objs]).


color(Params, Children) ->
    color_(Params, children(Children)).
color_(Params, Objs) ->
    Color = case proplists:get_value(c, Params) of
		[R,G,B] -> {R,G,B};
		[R,G,B,_A] -> {R,G,B}
	    end,
    w_color(Color, Objs).

%% ["h","r","r1","r2","center","d","d1","d2"],
cylinder(Params) ->
    H = proplists:get_value(h, Params, 1.0),
    D = proplists:get_valuer(d1, Params, undef),
    D1 = proplists:get_valuer(d1, Params, undef),
    D2 = proplists:get_valuer(d2, Params, undef),
    R  = proplists:get_valuer(r, Params, undef),
    R1 = proplists:get_valuer(r1, Params, undef),
    R2 = proplists:get_valuer(r2, Params, undef),

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
		     false -> [RR, RR,H/2];
		     true -> [0.0,0.0,0.0]
		 end,
    Fn = proplists:get_value('$fn', Params, 0),
    Ns = fragments(RR, Fn, 2, 12),
    Thickness = 1, %% scad parameter?
    OptList = [{cylinder_type,cylinder}, 
	       {sections,Ns}, 
	       {height,H},
	       {thickness, Thickness},
	       {rot_x,0},{rot_y,0},{rot_z,0},
	       {mov_x,Dx}, {mov_y,Dy}, {mov_z,Dz},
	       {top_x, RR1}, {top_z, RR1}, {bottom_x, RR2}, {bottom_z, RR2},
	       {ground, false}],
    plugin_call({cylinder, OptList}).


cube(Params) ->
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
    plugin_call({cube,OptList}).

sphere(Params) ->
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
    Fn = proplists:get_value('$fn', Params, 0),
    Fa = proplists:get_value('$fa', Params, 12),
    Fs = proplists:get_value('$fs', Params, 2),
    Ns = fragments(R, Fn, Fs, Fa),
    Nl = max(Ns div 2, 3),
    Xr = R,
    Yr = R,
    OptList = [{rot_x,0.0},{rot_y,0.0},{rot_z,0.0},
	       {mov_x,0.0}, {mov_y,0.0}, {mov_z,0.0},
	       {ground, false}],
    plugin_call({sphere,[Ns,Nl,Xr,Yr | OptList]}).

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

action_cast(Cast) ->
    wings_pid() ! {action, Cast}.

external_call(Call) ->
    Ref = make_ref(),   %% fixme: handle wings crash? monitor
    XCall = {call,Call,{self(),Ref}},
    wings_pid() ! {external, fun(St) -> command(XCall, St) end},
    receive
	{Ref,{exception,Error}} -> error(Error);
	{Ref,Result} -> Result
    end.

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
    

%% -include("mini_wings.hrl").
shapes(St) ->
    element(2, St).

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
command_({call,{cube,Arg},From}, St) ->
    try wpc_ncube:command({shape,{ncube, Arg}}, St) of
	{new_shape,Name,Fs,Vs} ->
	    ?dbg("new_shape: ~p ~p ~p\n", [Name,Fs,Vs]),
	    We = wings_we:build(Fs, Vs),
	    St1 = wings_obj:new(Name, We, St),
	    {ID,_} = get_latest_id_(St1),
	    reply(From, ID),
	    St1
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{cube, Arg}}}),
	    keep
    end;
command_({call,{cylinder,Arg},From}, St) ->
    try wings_shapes:command({cylinder, Arg}, St) of
	St1 ->
	    {ID,_} = get_latest_id_(St1),
	    reply(From, ID),
	    St1
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{cylinder, Arg}}}),
	    keep
    end;
command_({call,{sphere,Arg},From}, St) ->
    try wings_shapes:command({sphere, Arg}, St) of
	St1 ->
	    {ID,_} = get_latest_id_(St1),
	    reply(From, ID),
	    St1
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{sphere, Arg}}}),
	    keep
    end;
command_({call,{set_color,[Color,Objs]},From}, St) ->
    try wings_obj:update(
	  fun(We) ->
		  wings_va:set_body_color(Color, We)
	  end, Objs, St) of
	St1 ->
	    reply(From, Objs),
	    St1
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{color,[Color,Objs]}}}),
	    keep
    end;
command_({call,{transform,[Matrix,Objs]},From}, St) ->
    try wings_obj:update(
	  fun(We) ->
		  wings_we:transform_vs(Matrix, We)
	  end, Objs, St) of
	St1 ->
	    reply(From, Objs),
	    St1
    catch
	error:_ ->
	    reply(From, {exception,{badarg,{transform,[Matrix,Objs]}}}),
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
