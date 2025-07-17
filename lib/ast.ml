(* 关键字 *)
type keyword =
  Var | Const | Fn | If | While

(* 一元运算符 *)
type unaryop =
  Plus | Minus | LogicalNot

(* 二元运算符 *)
and binop =
  Add | Sub | Mul | Div | Mod (* 算术运算符 *)
  | Less | Le | Equal | Ge | Greater | NotEq (* 比较运算符 *)
  | LogicalAnd | LogicalOr (* 逻辑运算符 *)
  | Assign (* 赋值运算符 *)

(* 表达式 *)
type expr =
  Unit
  | Number of float
  | String of string
  | Identifier of string
  | UnaryExpr of unaryop * expr
  | BinExpr of expr * binop * expr
  | FnExpr of {
      params: (expr * type_annotation) list;
      return: type_annotation;
      statements: statement list;
      tail_expr: expr;
    }
  | IfExpr of {
      guard: expr;
      then_branch: statement list;
      then_tail_expr: expr;
      else_branch: statement list;
      else_tail_expr: expr;
    }

(* 类型注解 *)
and type_annotation =
  Unit
  | Type of string

(* 语句 *)
and statement =
  AssignStmt of {
    qualifier: [`Var | `Const];
    lhs: expr;
    rhs: expr;
  }
  | WhileStmt of {
      condition: expr;
      statements: statement list;
    }

(* 整个程序 *)
type prog = Prog of statement list
