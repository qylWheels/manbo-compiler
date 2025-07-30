{
  open Tokens
	exception Lexer_error of string
}

(* 基本字符 *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* 基本元素 *)
let word = (alpha | '_') (alpha | digit | '_')*
let int_literal = '-'? digit+
let float_literal = '-'? digit+ ('.' digit+)?
let str_literal_char = [^ '"' '\\' '\n']

rule token = parse
  (* 跳过空白字符 *)
  | [' ' '\t' '\n'] { token lexbuf }

  (* 关键字 *)
  | "var" { KEYWORD_VAR }
  | "const" { KEYWORD_CONST }
  | "fn" { KEYWORD_FN }
  | "return" { KEYWORD_RETURN }
  | "if" { KEYWORD_IF }
  | "else" { KEYWORD_ELSE }
  | "while" { KEYWORD_WHILE }

  (* 字面量 *)
  | int_literal as i { INT_LITERAL (int_of_string i) }
  | float_literal as f { FLOAT_LITERAL (float_of_string f) }
  | '"' (str_literal_char* as s) '"' { STRING_LITERAL s }

  (* 多字符符号 *)
  | "<=" { LE }
  | "==" { EQ }
  | ">=" { GE }
  | "!=" { NEQ }
  | "&&" { LOGICAL_AND }
  | "||" { LOGICAL_OR }

  (* 单字符符号 *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | '%' { PERCENT }
  | "!" { EXCLAIMATION }
  | '=' { EQUAL }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '<' { L_ANGLE_BRACKET }
  | '>' { R_ANGLE_BRACKET }

  (* 单行注释，词法分析的时候直接忽略即可 *)
  | "//" [^ '\n']* { token lexbuf }

  (* 标识符、类型 *)
  | word as w { WORD w }

  | eof { EOF }
