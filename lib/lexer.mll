{
  open Parser
	exception Lexer_error of string
}

(* 基本字符 *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* 一元运算符 *)
let plus = '+'
let minus = '-'

(* 基本元素 *)
let num_literal = (plus | minus)? digit+ ('.' digit+)?
let identifier = (alpha | '_') (alpha | digit | '_')*
let string_char = [^ '"' '\\' '\n']

rule token = parse
  (* 跳过空白字符 *)
  | [' ' '\t' '\n'] { token lexbuf }

  (* 关键字 *)
  | "var" { KEYWORD_VAR }
  | "const" { KEYWORD_CONST }
  | "fn" { KEYWORD_FN }
  | "if" { KEYWORD_IF }
  | "else" { KEYWORD_ELSE }
  | "while" { KEYWORD_WHILE }

  (* 元组字面量 *)
  | "()" { UNIT_LITERAL }

  (* 数字字面量 *)
  | num_literal as n { NUM_LITERAL (float_of_string n) }

  (* 字符串字面量 *)
  | '"' (string_char* as s) '"' { STRING_LITERAL s }

  (* 一元运算符、算数运算符 *)
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | '%' { MOD }

  (* 比较运算符 *)
  | '<' { LESS }
  | "<=" { LE }
  | "==" { EQUAL }
  | ">=" { GE }
  | '>' { GREATER }
  | "!=" { NOT_EQ }

  (* 逻辑运算符 *)
  | "&&" { LOGICAL_AND }
  | "||" { LOGICAL_OR }
  | "!" { LOGICAL_NOT }

  (* 赋值运算符 *)
  | '=' { ASSIGN }

  (* 括号 *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }

  (* 分号 *)
  | ';' { SEMICOLON }

  (* 冒号 *)
  | ':' { COLON }

  (* 逗号 *)
  | ',' { COMMA }

  (* 单行注释 *)
  | "//" [^ '\n']* { token lexbuf }

  (* 标识符、类型 *)
  | identifier as id { ID id }

  | eof { EOF }
