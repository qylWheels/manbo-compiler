%{
	open Ast
%}

%start <Ast.prog> prog

%%

prog:
	| stmts = statements; EOF { Prog stmts }

%%
