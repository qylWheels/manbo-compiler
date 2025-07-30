%%

%public expression:
	| LPAREN; RPAREN { Unit }
	| LPAREN; elems = tuple_elems; RPAREN { Tuple elems }
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

tuple_elems:
	| e = expression; option(COMMA) { [e] }
	| first = expression; COMMA; rest = tuple_elems { first :: rest }

call_args:
	| e = expression; option(COMMA) { [e] }
	| first = expression; COMMA; rest = call_args { first :: rest }
