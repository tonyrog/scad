# scad

OpenSCAD parser for Erlang

A yecc/leex parser.

    > scad:parse_file("example/example_1.scad").
	{ok,[{mcall,2,[],
            {id,2,"cube"},
            [{vector,2,[{number,2,8.0},{number,2,8.0},{number,2,8.0}]},
             {'=',{id,2,"center"},{true,2}}],
            empty}]}
