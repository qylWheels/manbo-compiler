%{
	open Ast
%}

%start <Ast.prog> prog

%%

prog:
	| items = list(item); EOF { items }

%%
