%%

%public statements:
	| stmt = statement { [stmt] }
	| first = statement; rest = statements { first :: rest }

%public statement:
	| q = qualifier; lhs = ident; EQUAL; rhs = expression; SEMICOLON {
			DefStmt { qualifier = q; lhs; rhs }
		}
	| lhs = ident; EQUAL; rhs = expression; SEMICOLON {
			AssignStmt { lhs; rhs }
		}
	| KEYWORD_WHILE; cond = expression; LBRACE; stmts = statements; RBRACE {
			WhileStmt { condition = cond; statements = stmts }
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

qualifier:
	| KEYWORD_VAR { Var }
	| KEYWORD_CONST { Const }
