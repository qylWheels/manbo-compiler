%{
	open Ast
%}

(* token集合 *)
(* 关键字 *)
%token KEYWORD_VAR
%token KEYWORD_CONST
%token KEYWORD_FN
%token KEYWORD_IF
%token KEYWORD_ELSE
%token KEYWORD_WHILE

(* 字面量 *)
%token <float> NUM_LITERAL
%token <string> STRING_LITERAL
%token UNIT_LITERAL

(* 变量名、类型名 *)
%token <string> ID

(* 一元运算符、算数运算符 *)
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD

(* 比较运算符 *)
%token LESS
%token LE
%token EQUAL
%token GE
%token GREATER
%token NOT_EQ

(* 逻辑运算符 *)
%token LOGICAL_AND
%token LOGICAL_OR
%token LOGICAL_NOT

(* 赋值运算符 *)
%token ASSIGN

(* 标点符号 *)
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token SEMICOLON
%token COLON
%token COMMA

(* 优先级 *)
%left LOGICAL_OR
%left LOGICAL_AND
%nonassoc EQUAL NOT_EQ
%nonassoc LESS LE GE GREATER
%left ADD SUB
%left MUL DIV MOD
%right LOGICAL_NOT
%nonassoc UMINUS (* 负号 *)

%start <Ast.prog> prog

%%

prog:
	| stmts = statements { Prog stmts }

statements:
	| stmt = statement { [stmt] }
	| first = statement; SEMICOLON; rest = statements { first :: rest }

statement:
	(* 赋值语句 *)
	| q = qualifier; lhs = expression; ASSIGN; rhs = expression; SEMICOLON {
			AssignStmt { qualifier = q; lhs; rhs }
		}

	(* while语句 *)
	| KEYWORD_WHILE; cond = expression; LBRACE; stmts = statements; RBRACE {
			WhileStmt { condition = cond; statements = stmts }
		}

expression:
	| LPAREN; RPAREN { Unit }
	| n = NUM_LITERAL { Number n }
	| s = STRING_LITERAL { String s }
	| id = ID { Identifier id }

	(* 括号表达式 *)
	| LPAREN; e = expression; RPAREN { e }

	(* 一元表达式 *)
	| SUB; e = expression; %prec UMINUS { UnaryExpr (Minus, e) }
	| LOGICAL_NOT; e = expression { UnaryExpr (LogicalNot, e) }

	(* 二元表达式 *)
	| e1 = expression; ADD; e2 = expression { BinExpr (e1, Add, e2) }
	| e1 = expression; SUB; e2 = expression { BinExpr (e1, Sub, e2) }
	| e1 = expression; MUL; e2 = expression { BinExpr (e1, Mul, e2) }
	| e1 = expression; DIV; e2 = expression { BinExpr (e1, Div, e2) }
	| e1 = expression; MOD; e2 = expression { BinExpr (e1, Mod, e2) }
	| e1 = expression; LESS; e2 = expression { BinExpr (e1, Less, e2) }
	| e1 = expression; LE; e2 = expression { BinExpr (e1, Le, e2) }
	| e1 = expression; EQUAL; e2 = expression { BinExpr (e1, Equal, e2) }
	| e1 = expression; GE; e2 = expression { BinExpr (e1, Ge, e2) }
	| e1 = expression; GREATER; e2 = expression { BinExpr (e1, Greater, e2) }
	| e1 = expression; NOT_EQ; e2 = expression { BinExpr (e1, NotEq, e2) }
	| e1 = expression; LOGICAL_AND; e2 = expression { BinExpr (e1, LogicalAnd, e2) }
	| e1 = expression; LOGICAL_OR; e2 = expression { BinExpr (e1, LogicalOr, e2) }

	| KEYWORD_FN; LPAREN; params = param_decls; RPAREN; COLON; ret_type = typ LBRACE;
		stmts = statements; e = expression;
		RBRACE {
			FnExpr { params; return = ret_type; statements = stmts; tail_expr = e }
		}
	| KEYWORD_IF; e = expression; LBRACE;
		then_stmts = statements; then_tail_expr = expression;
		RBRACE; KEYWORD_ELSE; LBRACE;
		else_stmts = statements; else_tail_expr = expression;
		RBRACE {
			IfExpr {
				guard = e;
				then_branch = then_stmts; then_tail_expr;
				else_branch = else_stmts; else_tail_expr
			}
		}

qualifier:
	| KEYWORD_VAR { `Var }
	| KEYWORD_CONST { `Const }

param_decls:
	| p = param_decl { [p] }
	| first = param_decl; COMMA; rest = param_decls { first :: rest }

param_decl:
	| id = ID; COLON; ty = typ { (Identifier id, ty) }

(* TODO: 提升type生成的扩展性 *)
typ:
	| UNIT_LITERAL { Unit : type_annotation }
	| t = ID { Type t }

%%
