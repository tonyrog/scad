%% hierarical version of car
-module(car_h).

-export([car/0]).

-include("scad_api.hrl").

%% -define('$fa', 1).
%% -define('$fs', 0.4).
-define(wheel_radius, 10).
-define(base_height, 10).
-define(top_height, 14).
-define(track, 35).
-define(axle_turns, 0).
%% -define(wheel_width, 10).
-define(wheel_width, 3).
 %% sinc wings has a tilted coordinate system compared to OpenScad
-define(body_roll, 270). 
-define(wheels_turn, 0).
-define(side_spheres_radius, 50).
-define(hub_thickness, 4).
-define(cylinder_radius, 2).
-define(cylinder_height, 2*?wheel_radius).

car() ->
    scope([{'$fa',1},{'$fs',0.4}],
	  body([0,0,0], [?body_roll,90,0],
	       [axle([-20,0,0], ?axle_turns, "front_axle", %% front axel
		     [
		      wheel([0,-?track/2,0], "front_left_wheel"),
		      wheel([0,?track/2,0],  "front_right_wheel")
		     ]),
		axle([20,0,0], 0.0, "rear_axle", %% rear axel
		     [
		      wheel([0,-?track/2,0], "rear_left_wheel"),
		      wheel([0,?track/2,0],  "rear_right_wheel")
		     ])
	       ])).

body(Pos,Roll,WheelAxis) ->
    translate(Pos, [{name, "car_pos"}],
	      rotate(Roll, [{name,"car"}],
		     [
		      %% Car body base
		      cube([60,20,?base_height],[{center,true},
						 {name,"car_base"}],
			   WheelAxis),
		      %% Car body top
		      translate([5,0,?base_height/2+?top_height/2 - 0.001],
				cube([30,20,?top_height],[{center,true},
							  {name,"car_top"}]))
		     ])).

axle(Vec, Turn, Name, Wheels) ->
    translate(Vec,
	      rotate([0,0,Turn], [{name,Name}],
		     cylinder(?track,2,[{center,true}],
			      Wheels))).

wheel(Vec, Name) ->
    translate(Vec,[{name,Name}],
	      rotate([0,0,0],
		     cylinder(?wheel_width,?wheel_radius,[{center,true}]))).
