%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    wings plugin to import OpenSCAD files.
%%% @end
%%% Created : 18 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(wpc_scad).

-export([import/1]).
-export([string/1]).

-include("scad.hrl").

	
import(Filename) ->
    {ok, Stmts} = scad_lint:file(Filename),
    Objs = scad_eval:stmt_list(Stmts),
    emit_object_list(Objs),
    Objs.


string(Data) ->
    string(Data,[]).
string(Data, Opts) ->
    {ok, Stmts} = scad_lint:string(Data, Opts),
    Objs = scad_eval:stmt_list(Stmts),
    emit_object_list(Objs),
    Objs.

emit_object_list(Objs) ->
    emit_object_list(Objs,0).

emit_object_list([Obj|Objs],I) ->
    emit_object(Obj,I),
    emit_object_list(Objs,I);
emit_object_list([],_I) ->
    ok.

emit_object(#object{type=Type, params=Params, children=Children}, I) ->
    io:format("~s~s: ~w\n", [lists:duplicate(I,$\s),Type,Params]),
    emit_object_list(Children,I+2).
