%% -*- erlang -*-
%% OpenSCAD scanner
%%

Definitions.

D = [0-9]
E = [Ee][+-]?{D}+
H = [0-9a-fA-F]

U       = [\x80-\xbf]
U2      = [\xc2-\xdf]
U3      = [\xe0-\xef]
U4      = [\xf0-\xf4]
UNICODE = {U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
WS      = [\000-\s]

Rules.

module            : {token,{module,TokenLine}}.
function          : {token,{function,TokenLine}}.
if                : {token,{'if',TokenLine}}.
else              : {token,{'else',TokenLine}}.
let               : {token,{'let',TokenLine}}.
assert            : {token,{assert,TokenLine}}.
echo              : {token,{echo,TokenLine}}.
for               : {token,{for,TokenLine}}.
each              : {token,{each,TokenLine}}.
true              : {token,{true,TokenLine}}.
false             : {token,{false,TokenLine}}.
undef             : {token,{undef,TokenLine}}.
use[\s\t\r\n]*\<[^\t\r\n>]+\> : {token,{use,TokenLine,TokenChars}}.

"(\^.|\.|[^\"])*" : S = lists:sublist(TokenChars,2,TokenLen-2),
                   {token,{string, TokenLine, S}}.
[\xc2\xa0]+     : {error,TokenLine}.
{UNICODE}+      : {error,TokenLine}.

{D}+	         : {token,{number,TokenLine,TokenChars}}.
{D}+{E}	         : {token,{number,TokenLine,TokenChars}}.
{D}*\.{D}+({E})? : {token,{number,TokenLine,TokenChars}}.
{D}+\.{D}*({E})? : {token,{number,TokenLine,TokenChars}}.
\$?[a-zA-Z0-9_]+  : {token,{id,TokenLine,TokenChars}}.

<=               : {token,{'<=',TokenLine}}.
>=               : {token,{'>=',TokenLine}}.
==               : {token,{'==',TokenLine}}.
!=               : {token,{'!=',TokenLine}}.
&&               : {token,{'&&',TokenLine}}.
\|\|             : {token,{'||',TokenLine}}.
;		: {token,{';',TokenLine}}.
{		: {token,{'{',TokenLine}}.
}		: {token,{'}',TokenLine}}.
,		: {token,{',',TokenLine}}.
:		: {token,{':',TokenLine}}.
=		: {token,{'=',TokenLine}}.
\(		: {token,{'(',TokenLine}}.
\)		: {token,{')',TokenLine}}.
\[		: {token,{'[',TokenLine}}.
\]		: {token,{']',TokenLine}}.
\.		: {token,{'.',TokenLine}}.
!		: {token,{'!',TokenLine}}.
-		: {token,{'-',TokenLine}}.
\+		: {token,{'+',TokenLine}}.
\*		: {token,{'*',TokenLine}}.
/		: {token,{'/',TokenLine}}.
\%		: {token,{'%',TokenLine}}.
<		: {token,{'<',TokenLine}}.
>		: {token,{'>',TokenLine}}.
\^		: {token,{'^',TokenLine}}.
\?		: {token,{'?',TokenLine}}.
{WS}+            : skip_token.


Erlang code.

