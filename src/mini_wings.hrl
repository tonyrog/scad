-ifndef(__MINI_WINGS_HRL__).
-define(__MINI_WINGS_HRL__, true).

%% State and records
%% Main state record containing all objects and other important state.
-record(st,
	{shapes=gb_trees:empty() :: gb_trees:tree(),%All visible objects
	 selmode=face :: wings_sel:mode(),          %Selection mode.
	 sh=false :: boolean(),			    %Smart highlighting active.
	 sel=[],                     %Current sel: [{Id,GbSet}]
	 ssels=gb_trees:empty() :: gb_trees:tree(), %Saved selections:

	 %% Selection only temporary?
	 temp_sel=none :: 'none' | {wings_sel:mode(),boolean()},

	 mat=gb_trees:empty() :: gb_trees:tree(),%Defined materials (GbTree).
	 pal=[],                                %Palette
	 file,					%Current filename.
	 saved=false :: 'false'  | 'true' | 'auto' | integer(),
	 onext=1 :: pos_integer(),		%Next object id to use.

	 %% Saved bounding box. (AutoUV uses it for its own purposes,
	 %% therefore the type must also a allow a tuple.)
	 bb=none,

	 edge_loop=none,			%Previous edge loop.
	 views={0,{}},				%{Current,TupleOfViews}
	 pst=gb_trees:empty() :: gb_trees:tree(), %Plugin State Info
						%   gb_tree where key is plugin	module

	 %% Previous commands.
	 repeatable=ignore,                     %Last repeatable command.
	 ask_args,				%Ask arguments.
	 drag_args,			        %Drag arguments for command.

	 %% Default commands (LMB, RMB).
	 def={ignore,ignore},


	 %% Undo information.
	 last_cmd=empty_scene,		        %Last command.
	 undo=queue:new() :: queue:queue(),	%Undo (de)queue.
	 next_is_undo=true :: boolean(),	%State of undo/redo toggle.
	 undone=[] :: list()		        %States that were undone.
	}).

-record(we,
	{id :: non_neg_integer()|undefined,	%Shape id. (undefined during construction)
	 perm=0 :: wings_we:perm(),             %See wings_we.erl.
	 name="" :: string() | tuple(),		%Name. (AutoUV stores other things here.)
	 es=array:new() :: array:array(),	%array containing edges
	 lv=none :: 'none' | array:array(),	%Left vertex attributes
	 rv=none :: 'none' | array:array(),	%Right vertex attributes,
	 fs :: gb_trees:tree() | undefined,	%Faces (undefined during construction)
	 he=gb_sets:empty() :: gb_sets:set(),	%Hard edges
	 vc :: array:array() | undefined,       %Connection info (=incident edge)
						% for vertices. (undefined during re-construction)
	 vp=array:new() :: array:array(),	%Vertex positions.
	 pst=gb_trees:empty(),                  %Plugin State Info, 
						%   gb_tree where key is plugin module
	 mat=default,				%Materials.
	 next_id=0 :: non_neg_integer(),	%Next free ID for vertices,
						% edges, and faces.
						% (Needed because we never re-use
						%  IDs.)
	 mirror=none :: 'none' | non_neg_integer(),	%Mirror: none|Face
	 light=none,				%Light data: none|Light
	 holes=[] :: [integer()],		%List of hole faces.
         temp=[] :: term()
	}).

-endif.
