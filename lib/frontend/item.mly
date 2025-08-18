%%

%public item:
	| stmt = statement { StmtItem stmt }
	| s = struct_def { s }
	| fn = function_def { fn }

struct_def:
	| KEYWORD_STRUCT; id = ident; LBRACE; fields = separated_list(SEMICOLON, struct_field); RBRACE {
			StructItem { ident = id; fields }
		}

struct_field:
	| id = ident; COLON; ty = typ { (id, ty) }

function_def:
	| KEYWORD_FN; id = ident; LPAREN;
		params = option(fn_params);
		RPAREN; ret = option(fn_ret) LBRACE;
		stmts = option(statements);
		RBRACE {
			FnItem {
				ident = id;
				params = Option.value params ~default:[];
				return = Option.value ret ~default:(UnitType);
				statements = Option.value stmts ~default:[];
			}
		}

fn_params:
	| p = fn_param; option(COMMA) { [p] }
	| first = fn_param; COMMA; rest = fn_params { first :: rest }

fn_param:
	| id = ident; COLON; ty = typ { (id, ty) }

fn_ret:
	| ty = typ { ty }
