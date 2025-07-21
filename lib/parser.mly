%{
	open Ast
%}

(* 关键字 *)
%token KEYWORD_VAR
%token KEYWORD_CONST
%token KEYWORD_FN
%token KEYWORD_RETURN
%token KEYWORD_IF
%token KEYWORD_ELSE
%token KEYWORD_WHILE

(* 字面量 *)
%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL

(* 多字符符号 *)
%token LE
%token EQ (* == *)
%token GE
%token NEQ
%token LOGICAL_AND
%token LOGICAL_OR

(* 单字符符号 *)
%token PLUS
%token MINUS
%token MUL
%token DIV
%token PERCENT
%token EXCLAIMATION
%token EQUAL (* = *)
%token SEMICOLON
%token COLON
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token L_ANGLE_BRACKET
%token R_ANGLE_BRACKET

(* 单词 *)
%token <string> WORD

%token EOF

(* 优先级 *)
%left LOGICAL_OR
%left LOGICAL_AND
%nonassoc EQ NEQ
%nonassoc L_ANGLE_BRACKET LE GE R_ANGLE_BRACKET
%left PLUS MINUS
%left MUL DIV PERCENT
%right EXCLAIMATION
%nonassoc UMINUS (* 负号 *)

%start <Ast.prog> prog

%%

prog:
	| stmts = statements; EOF { Prog stmts }

statements:
	| stmt = statement { [stmt] }
	| first = statement; rest = statements { first :: rest }

statement:
	| q = qualifier; lhs = ident; EQUAL; rhs = expression; SEMICOLON {
			DefStmt { qualifier = q; lhs; rhs }
		}
	| lhs = ident; EQUAL; rhs = expression; SEMICOLON {
			AssignStmt { lhs; rhs }
		}
	| KEYWORD_WHILE; cond = expression; LBRACE; stmts = statements; RBRACE {
			WhileStmt { condition = cond; statements = stmts }
		}
	| KEYWORD_FN; id = ident; LPAREN;
		params = option(fn_params);
		RPAREN; ret_type = option(fn_ret_type) LBRACE;
		stmts = option(statements);
		RBRACE {
			FnStmt {
				ident = id;
				params = Option.value params ~default:[];
				return = Option.value ret_type ~default:(Unit : typ);
				statements = Option.value stmts ~default:[];
			}
		}
	| KEYWORD_RETURN; e = option(expression); SEMICOLON {
			RetStmt (Option.value e ~default:(Unit : expr))
		}
	| KEYWORD_IF; e = expression; LBRACE;
		then_branch = option(statements)
		RBRACE; KEYWORD_ELSE; LBRACE;
		else_branch = option(statements)
		RBRACE {
			IfStmt {
				guard = e;
				then_branch = Option.value then_branch ~default:[];
				else_branch = Option.value else_branch ~default:[];
			}
		}

expression:
	| LPAREN; RPAREN { Unit }
	| i = INT_LITERAL { Integer i }
	| f = FLOAT_LITERAL { Float f }
	| s = STRING_LITERAL { String s }
	| id = ident { Identifier id }

	(* 括号表达式 *)
	| LPAREN; e = expression; RPAREN { e }

	(* 一元表达式 *)
	| MINUS; e = expression; %prec UMINUS { UnaryExpr (Minus, e) }
	| EXCLAIMATION; e = expression { UnaryExpr (LogicalNot, e) }

	(* 二元表达式 *)
	| e1 = expression; PLUS; e2 = expression { BinExpr (e1, Add, e2) }
	| e1 = expression; MINUS; e2 = expression { BinExpr (e1, Sub, e2) }
	| e1 = expression; MUL; e2 = expression { BinExpr (e1, Mul, e2) }
	| e1 = expression; DIV; e2 = expression { BinExpr (e1, Div, e2) }
	| e1 = expression; PERCENT; e2 = expression { BinExpr (e1, Mod, e2) }
	| e1 = expression; L_ANGLE_BRACKET; e2 = expression { BinExpr (e1, Less, e2) }
	| e1 = expression; LE; e2 = expression { BinExpr (e1, Le, e2) }
	| e1 = expression; EQ; e2 = expression { BinExpr (e1, Equal, e2) }
	| e1 = expression; GE; e2 = expression { BinExpr (e1, Ge, e2) }
	| e1 = expression; R_ANGLE_BRACKET; e2 = expression { BinExpr (e1, Greater, e2) }
	| e1 = expression; NEQ; e2 = expression { BinExpr (e1, NotEq, e2) }
	| e1 = expression; LOGICAL_AND; e2 = expression { BinExpr (e1, LogicalAnd, e2) }
	| e1 = expression; LOGICAL_OR; e2 = expression { BinExpr (e1, LogicalOr, e2) }

	(* 函数调用表达式 *)
	| id = ident; LPAREN; args = option(call_args); RPAREN {
			CallExpr (id, Option.value args ~default:[])
		}

call_args:
	| e = expression; option(COMMA) { [e] }
	| first = expression; COMMA; rest = call_args { first :: rest }

qualifier:
	| KEYWORD_VAR { `Var }
	| KEYWORD_CONST { `Const }

ident:
	| w = WORD { Identifier w : identifier }

fn_params:
	| p = fn_param; option(COMMA) { [p] }
	| first = fn_param; COMMA; rest = fn_params { first :: rest }

fn_param:
	| id = ident; COLON; ty = typ { (id, ty) }

fn_ret_type:
	| COLON; ty = typ { ty }

typ:
	| LPAREN; RPAREN { Unit : typ }
	| w = WORD { Type w }

%%
