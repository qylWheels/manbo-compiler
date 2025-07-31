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
%token DOT
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

%start <unit> dummy

%%

dummy:
	| EOF { () }
