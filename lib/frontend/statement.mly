%%

%public statements:
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

qualifier:
	| KEYWORD_VAR { Var }
	| KEYWORD_CONST { Const }

fn_params:
	| p = fn_param; option(COMMA) { [p] }
	| first = fn_param; COMMA; rest = fn_params { first :: rest }

fn_param:
	| id = ident; COLON; ty = typ { (id, ty) }


