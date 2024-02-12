%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Hierarchical robot + control
%%% @end
%%% Created : 11 Feb 2024 by Tony Rogvall <tony@rogvall.se>

-module(robot_h).

-include("scad_api.hrl").

-export([model/0,model/1]).
-compile(export_all).


-define(default_height, 18).
-define(torso_width(H),   (0.258*(H))).
-define(torso_depth(H),   (0.200*(H))).
-define(torso_height(H),  ((0.870-0.485)*(H))).
-define(head_radius(H),   (0.150*(H))).
-define(leg_length(H),    (0.530*(H))). %% upper + lower
-define(arm_length(H),    ((0.870-0.630)*(H))). %% upper + lower

-define(upper_leg_length(H), ((0.530-0.285)*(H))).
-define(upper_leg_radius(H), 6).
-define(lower_leg_length(H), ((0.285)*(H))).
-define(lower_leg_radius(H), 5).

-define(upper_arm_length(H), ((0.870-0.818)*(H))).
-define(upper_arm_radius(H), 5).
-define(lower_arm_length(H), ((0.818-0.630)*(H))).
-define(lower_arm_radius(H), 4).
-define(hand_length(H),      ((0.630-0.485)*(H))).
-define(hand_radius(H),      4).

-define(foot_breadth(H),  (0.055*(H))).
-define(foot_length(H),   (0.152*(H))).

model() ->
    model(?default_height).
model(H) ->
    scope([{'$fa',1},{'$fs',0.4}],
	  torso(H, [0,0,0], 0, "robot")).

torso(H, _Pos=[X,_,Z], Angle, Name) ->
    translate([X,?leg_length(H)+?torso_height(H)/2,Z],
	      rotate([Angle,0,0], [{name,Name}],
		     cube([?torso_width(H),?torso_height(H),?torso_depth(H)],
			  [{center,true},{name,"torso"}],
			  [translate([0,?torso_height(H)/2-1,0],
				     rotate([0,0,90], [{name,"arm_axis"}],
					    cylinder(?torso_width(H)+2,0.5,[{center,true}]))),
			   translate([0,-?torso_height(H)/2+1,0],
				     rotate([0,0,90], [{name,"leg_axis"}],
					    cylinder(?torso_width(H)+2,0.5,[{center,true}]))),
			   head(H, [0,?torso_height(H)/2+?head_radius(H)/2,0], 0.0, "head")
			  ]
			 ))).

body_parts(H) ->
    [%% head(H, [0,?torso_height(H)/2,0], 0.0, "head"),
     arm(H, [-?torso_width(H),-?torso_height(H)/2,0],  0.0, "left_arm"),
     arm(H, [+?torso_width(H),-?torso_height(H)/2,0],  0.0, "right_arm"),
     leg(H, [-?torso_width(H),?torso_height(H)/2,0],   0.0, "left_leg"),
     leg(H, [+?torso_width(H),?torso_height(H)/2,0],  0.0, "right_leg")].

%% fixme, decorate the head with eyes, noose and mouth!
%% also add a neck
head(H, Pos, Angle, Name) ->
    translate(Pos,
	      rotate([0,Angle,0], [{name,Name}],
		     sphere(?head_radius(H),[{center,true}]))).

%% FIXME: make mulitple joins
arm(H, Pos, Angle, Name) ->
    translate(Pos,
	      rotate([Angle,0,0], [{name,Name}],
		     cylinder(?upper_arm_radius(H),?upper_arm_length(H),
			      [{center,true},{name,Name++".upper"}],
			      cylinder(?lower_arm_radius(H),?lower_arm_length(H),
				       [{center,true},{name,Name++".lower"}],
				       cylinder(?hand_radius(H),
						?hand_length(H),
						[{center,true},{name,Name++".hand"}]))))).

%% FIXME: make mulitple joins
leg(H, Pos, Angle, Name) ->
    translate(Pos,
	      rotate([Angle,0,0], [{name,Name}],
		     cylinder(?upper_leg_radius(H),?upper_leg_length(H),
			      [{center,true},{name,Name++"upper"}],
			      [cylinder(?lower_leg_radius(H),?lower_leg_length(H),
					[{center,true},{name,Name++"lower"}],
					[cylinder(?foot_breadth(H),?foot_length(H),
						  [{center,true},{name,Name++"foot"}])])]))).

