%%

%public typ:
	| LPAREN; RPAREN { UnitType }
	| LPAREN; elem_tys = tuple_elem_tys; RPAREN; { TupleType elem_tys }
	| w = WORD { Type w }
	| KEYWORD_FN; LPAREN; params = option(fn_param_types); RPAREN; return = option(fn_ret_type) {
		FnType {
			params = Option.value params ~default:[];
			return = Option.value return ~default:(UnitType)
		}
	}

tuple_elem_tys:
	| ty = typ; option(COMMA) { [ty] }
	| first = typ; option(COMMA); rest = tuple_elem_tys { first :: rest }

fn_param_types:
	| fpt = fn_param_type; option(COMMA) { [fpt] }
	| first = fn_param_type; COMMA; rest = fn_param_types { first :: rest }

fn_param_type:
	| t = typ { t }

fn_ret_type:
	| ty = typ { ty }
