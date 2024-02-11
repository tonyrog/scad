%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    OpenScad parser
%%% @end
%%% Created : 15 Jan 2024 by Tony Rogvall <tony@rogvall.se>

-module(scad).

-export([tokens/1]).
-export([parse_file/1, parse_string/1]).
-export([scan_file/1]).
-export([remove_comments/1]).
-export([read_file/1]).

-define(log(F,A), ok).

parse_file(File) ->
    case read_file(File) of
	{ok,Binary} ->
	    parse_file_data(File,Binary);
	Error={error,_Reason} ->
	    ?log("Unable to read file ~s (~p)\n",[File,_Reason]),
	    Error
    end.

parse_string(Binary) when is_binary(Binary) ->
    parse_string_(binary_to_list(Binary));
parse_string(String) when is_list(String) ->
    parse_string_(String).

parse_string_(String) ->
    {ok,Ts} = tokens(String),
    scad_parse:parse(Ts).

parse_file_data(File, String) ->
    parse_file_data(File, String, #{}).

parse_file_data(File, Binary, Opts) when is_binary(Binary) ->
    parse_file_data(File, binary_to_list(Binary), Opts);    
parse_file_data(File, String, _Opts) ->
    case tokens(String) of
	{ok,Ts} ->
	    case scad_parse:parse(Ts) of
		{ok,Ast} ->
		    {ok, Ast};
		Error={error,{Ln,Mod,Why}} when 
		      is_integer(Ln), is_atom(Mod) ->
		    Reason = Mod:format_error(Why),
		    io:format("~s:~w: ~s\n", [File,Ln,Reason]),
		    Error;
		Error ->
		    io:format("~s: Error: ~p\n", [File,Error]),
		    Error
	    end;
	Error ->
	    io:format("~s: Error: ~p\n", [File, Error]),
	    Error
    end.

scan_file(File) ->
    case read_file(File) of
	{ok,Binary} ->    
	    tokens(binary_to_list(Binary));
	Error={error,Reason} ->
	    io:format("Unable to read file ~s (~w)\n",
		      [File, Reason]),
	    Error
    end.

tokens(String) ->
    case scad_scan:string(remove_comments(String)) of
	{ok,Ts,_Ln} -> {ok,Ts};
	Error -> Error
    end.

%% remove C-style comments from data
remove_comments([$/,$/|Cs]) -> remove_comments(remove_line(Cs));
remove_comments([$/,$*|Cs]) -> remove_comments(remove_block(Cs));
remove_comments([C|Cs]) -> [C|remove_comments(Cs)];
remove_comments([]) -> [].

%% remove until */ but keep all \n
remove_block([$*,$/|Cs]) -> Cs;
remove_block([$\n|Cs]) -> [$\n|remove_block(Cs)];
remove_block([_|Cs]) -> remove_block(Cs);
remove_block([]) -> [].

%% remove until end-of-line (but keep it)
remove_line(Cs=[$\n|_]) -> Cs;
remove_line([_|Cs]) -> remove_line(Cs);
remove_line([]) -> [].


read_file(FileName) ->
    file:read_file(FileName).
