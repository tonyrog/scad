%% -*- erlang -*-
%% ANSI C parser 
%%
Nonterminals
   expr call logic_or logic_and equality comparison addition multiplication
   exponent unary primary list_comprehension_elements
   list_comprehension_elements_p vector_element expr_or_empty
   vector_elements module_instantiation
   if_statement ifelse_statement
   arguments argument_list parameters parameter_list
   argument parameter
   module_id
   input statement assignment inner_input child_statements 
   child_statement optional_trailing_comma
  .

Terminals
   module function if else for let assert echo each
   id string use include number true false undef '<=' '>=' '==' '!=' '||' '&&'
   '=' ';' '!' '#' '%' '*' '(' ')' '{' '}' '?' ':' '>' '<' '+' '-'
   '/' '^' '[' ']' '.' ',' .

Rootsymbol input.

%%

input -> '$empty' : [].
input -> input use : '$1' ++ ['$2'].
input -> input include : '$1' ++ ['$2'].
input -> input statement : '$1'++['$2'].

statement -> ';' : empty.
statement -> '{' inner_input '}' : {block,line('$1'),'$2'}.
statement -> assignment : '$1'.
statement -> 'module' id '('  parameters ')' statement :
		 { module, line('$1'), id('$2'), '$4', '$6' }.
statement -> 'function' id '(' parameters ')' '=' expr ';' :
		 { function, line('$1'), id('$2'), '$4', '$7' }.
statement -> module_instantiation : '$1'.


inner_input -> '$empty' : [].
inner_input -> inner_input statement : '$1' ++ ['$2'].

assignment -> id '=' expr ';' : {assign, line('$1'), id('$1'), '$3'}.

module_instantiation -> '!' module_instantiation : tag(root,'$2').
module_instantiation -> '#' module_instantiation : tag(highlight,'$2').
module_instantiation -> '%' module_instantiation : tag(background,'$2').
module_instantiation -> '*' module_instantiation : tag(delete, '$2').
module_instantiation -> module_id '(' arguments ')' child_statement :
			    {mcall,line('$1'),[],'$1','$3','$5'}.
module_instantiation -> ifelse_statement : '$1'.
    
ifelse_statement -> if_statement : '$1'.
ifelse_statement -> if_statement 'else' child_statement : 
			{'if', Line, Cond, Then} = '$1', 
		    {'if', Line, Cond, Then, '$3'}.

if_statement -> 'if' '(' expr ')' child_statement : 
		    {'if',line('$1'), '$3', '$5' }.

     
child_statements -> '$empty' : [].
child_statements -> child_statements child_statement : '$1' ++ ['$2'].
child_statements -> child_statements assignment : '$1' ++ ['$2'].

child_statement -> ';' : empty.
child_statement -> '{' child_statements '}' : {block,line('$1'),'$2'}.
child_statement -> module_instantiation : '$1'.

module_id -> id    : id('$1').
module_id -> 'for' : id('$1').
module_id -> 'let' : id('$1').
module_id -> 'assert' : id('$1').
module_id -> 'echo' : id('$1').
module_id -> 'each' : id('$1').

expr -> logic_or : '$1'.
expr -> 'function' '(' parameters ')' expr : 
	    {function,line('$1'),'$3','$5'}.
expr -> logic_or '?' expr ':' expr : {op,line('$2'),'?','$1','$3','$5'}.
expr -> 'let' '(' arguments ')' expr : {'let',line('$1'),'$3','$5'}.
expr -> 'assert' '(' arguments ')' expr_or_empty :
	    {'assert',line('$1'),'$3','$5'}.
expr -> 'echo' '(' arguments ')' expr_or_empty :
	    {'echo',line('$1'),'$3','$5'}.


logic_or -> logic_and : '$1'.
logic_or -> logic_or '||' logic_and : {op,line('$2'),'||','$1','$3'}.

logic_and -> equality : '$1'.
logic_and -> logic_and '&&' equality: {op,line('$2'),'&&','$1','$3'}.

equality -> comparison : '$1'.
equality -> equality '==' comparison : {op,line('$2'),'==','$1','$3'}.
equality -> equality '!=' comparison: {op,line('$2'),'!=','$1','$3'}.

comparison -> addition : '$1'.
comparison -> comparison '>' addition : {op,line('$2'),'>','$1','$3'}.
comparison -> comparison '>=' addition : {op,line('$2'),'>=','$1','$3'}.
comparison -> comparison '<' addition : {op,line('$2'),'<','$1','$3'}.
comparison -> comparison '<=' addition : {op,line('$2'),'<=','$1','$3'}.

addition -> multiplication : '$1'.
addition -> addition '+' multiplication : {op,line('$2'),'+','$1','$3'}.
addition -> addition '-' multiplication : {op,line('$2'),'-','$1','$3'}.

multiplication -> unary : '$1'.
multiplication -> multiplication '*' unary : {op,line('$2'),'*','$1','$3'}.
multiplication -> multiplication '/' unary : {op,line('$2'),'/','$1','$3'}.
multiplication -> multiplication '%' unary : {op,line('$2'),'%','$1','$3'}.

unary -> exponent : '$1'.
unary -> '+' unary : 
	     case '$2' of
		 {number,_Line,_Value} -> '$2';
		 _ -> {op,line('$1'),'+','$1'}
	     end.
unary -> '-' unary :
	     case '$2' of
		 {number,Line,Value} -> 
		     {number,Line,-Value};
		 _ ->
		     {op,line('$1'),'-','$1'}
	     end.
unary -> '!' unary : {op,line('$1'),'!','$1'}.

exponent -> call : '$1'.
exponent -> call '^' unary : {op,line('$2'),'^','$1','$3'}.

call -> primary : '$1'.
call -> call '(' arguments ')' : {op,line('$2'),call,'$1','$3'}.
call -> call '[' arguments ']' : {op,line('$2'),index,'$1','$3'}.
call -> call '.' id : {op,line('$1'),member,'$1','$3'}.

primary -> 'true'  : '$1'.
primary -> 'false' : '$1'.
primary -> 'undef' : '$1'.
primary -> number  : {number,line('$1'),to_number('$1')}.
primary -> string  : {string,line('$1'),value('$1')}.
primary -> id      : {id,line('$1'), value('$1')}.
primary -> '(' expr ')' : '$2'.
primary -> '[' expr ':' expr ']' : {range,line('$1'),'$2','$4'}.
primary -> '[' expr ':' expr ':' expr ']' : {range,line('$1'),'$2','$4','$6'}.
primary -> '[' ']' : {vector,line('$1'),[]}.
primary -> '[' vector_elements optional_trailing_comma ']' : 
	       {vector,line('$1'),'$2'}.

expr_or_empty -> '$empty' : empty.
expr_or_empty -> expr : '$1'.

list_comprehension_elements -> 
    'let' '(' arguments ')' list_comprehension_elements_p :
	{lc_let,line('$1'),'$3','$5'}.
list_comprehension_elements -> 
    'each' vector_element : {lc_each,line('$1'), '$2'}.
list_comprehension_elements -> 
    'for' '(' arguments ')' vector_element : {lc_for,line('$1'),'$3','$5'}.
list_comprehension_elements -> 
    'for' '(' arguments ';' expr ';' arguments ')' vector_element :
	{lc_forc,line('$1'),'$3','$5','$7','$9'}.
list_comprehension_elements -> 
    'if' '(' expr ')' vector_element :
	{lc_if,line('$1'),'$3','$5'}.
list_comprehension_elements -> 
    'if' '(' expr ')' vector_element 'else' vector_element :
	{lc_if,line('$1'),'$3','$5','$7'}.	

%% list_comprehension_elements with optional parenthesis
list_comprehension_elements_p ->
    list_comprehension_elements : '$1'.
list_comprehension_elements_p -> 
    '(' list_comprehension_elements ')' : '$2'.


optional_trailing_comma -> '$empty' : empty.
optional_trailing_comma -> ',' : empty.

vector_elements -> vector_element : ['$1'].
vector_elements -> vector_elements ',' vector_element : '$1'++['$3'].

vector_element -> list_comprehension_elements_p : '$1'.
vector_element -> expr : '$1'.

parameters -> '$empty' : [].
parameters -> parameter_list optional_trailing_comma : '$1'.

parameter_list -> parameter : ['$1'].
parameter_list -> parameter_list ',' parameter : '$1' ++ ['$3'].

parameter -> id : '$1'.
parameter -> id '=' expr : {'=',line('$1'),'$1','$3'}.

arguments -> '$empty' : [].
arguments -> argument_list optional_trailing_comma : '$1'.

argument_list -> argument : ['$1'].
argument_list -> argument_list ',' argument : '$1'++['$3'].

argument -> expr : '$1'.
argument -> id '=' expr : {'=',line('$1'),'$1','$3'}.


Erlang code.

id({id,Line,Name}) ->
    {id,Line,Name};
id({Tok,Line}) when is_atom(Tok) ->
    {id,Line,atom_to_list(Tok)}.

line({_,Line}) when is_integer(Line) -> Line;
line({_,Line,_}) when is_integer(Line) -> Line;
line({_,Line,_,_}) when is_integer(Line) -> Line;
line({_,Line,_,_,_}) when is_integer(Line) -> Line;
line({_,Line,_,_,_,_}) when is_integer(Line) -> Line.

value({_,_Line}) -> undefined;
value({_,_Line,Value}) -> Value.

tag(Tag,{mcall,Line,Tags,Module,Args,Stmts}) ->
    {mcall,Line,[Tag|Tags],Module,Args,Stmts};
tag(_Tag,Stmt) ->
    Stmt.
    

to_number({number,_Line,Value}) ->
    try list_to_integer(Value) of
	Int -> Int
    catch
	error:_ -> list_to_float(Value)
    end.
	    
     
    

    

    
    
    
    
    


    
