-module(car).

-export([car/0]).

-include("scad_api.hrl").

%% -define('$fa', 1).
%% -define('$fs', 0.4).
-define(wheel_radius, 10).
-define(base_height, 10).
-define(top_height, 14).
-define(track, 35).
%% -define(wheel_width, 10).
-define(wheel_width, 3).
-define(body_roll, 0).
-define(wheels_turn, 0).
-define(side_spheres_radius, 50).
-define(hub_thickness, 4).
-define(cylinder_radius, 2).
-define(cylinder_height, 2*?wheel_radius).

car() ->
    scope([{'$fa',1},{'$fs',0.4}],
	  [
	   body(?body_roll),
	   wheel([-20,-?track/2,0], ?wheels_turn, "front_left_wheel"),
	   wheel([-20,?track/2,0], ?wheels_turn, "front_right_wheel"),
	   wheel([20,-?track/2,0], 0.0, "rear_left_wheel"),
	   wheel([20,?track/2,0], 0.0, "rear_right_wheel"),
	   axle([-20,0,0]),
	   axle([20,0,0])
	  ]).

body(Roll) ->
    rotate([Roll,0,0],
	   [
	    %% Car body base
	    cube([60,20,?base_height],[{center,true},
				       {name,"body_base"}]),
	    %% Car body top
	    translate([5,0,?base_height/2+?top_height/2 - 0.001],
		      cube([30,20,?top_height],[{center,true},
					       {name,"body_top"}]))
	   ]).

axle(Vec) ->
    translate(Vec,
	      rotate([0,0,0],
		     cylinder(?track,2,[{center,true}]))).

wheel(Vec, Turn, Name) ->
    translate(Vec,[{name,Name}],
	      rotate([Turn,0,0],
		     cylinder(?wheel_width,?wheel_radius,[{center,true}]))).
    
wheel_1(Vec, Turn, Name) ->
    translate(Vec,[{name,Name}],
	      rotate([Turn,0.0,0.0],
		     difference(
		       [%% Wheel sphere
			sphere(?wheel_radius),
			%% Side sphere 1
			translate([0,?side_spheres_radius + ?hub_thickness/2,0], sphere(?side_spheres_radius)),
			%% Side sphere 2
			translate([0,- (?side_spheres_radius + ?hub_thickness/2),0], sphere(?side_spheres_radius)),
			%% Cylinder 1
			translate([?wheel_radius/2,0,0],rotate([90,0,0],cylinder(?cylinder_height,?cylinder_radius,[{center,true}]))),
			%% Cylinder 2
			translate([0,0,?wheel_radius/2],rotate([90,0,0],cylinder(?cylinder_height,?cylinder_radius,[{center,true}]))),
			%% Cylinder 3
			translate([-?wheel_radius/2,0,0],rotate([90,0,0],cylinder(?cylinder_height,?cylinder_radius,[{center,true}]))),
			   %% Cylinder 4
			translate([0,0,-?wheel_radius/2],rotate([90,0,0],cylinder(?cylinder_height,?cylinder_radius,[{center,true}])))
		       ]))).
