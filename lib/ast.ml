(* 关键字 *)
type keyword =
  Var | Const | Fn | Return | If | Else | While [@@deriving show]

(* 一元运算符 *)
type unaryop =
  Minus | LogicalNot [@@deriving show]

(* 二元运算符 *)
and binop =
  Add | Sub | Mul | Div | Mod (* 算术运算符 *)
  | Less | Le | Equal | Ge | Greater | NotEq (* 比较运算符 *)
  | LogicalAnd | LogicalOr (* 逻辑运算符 *)
  | Assign (* 赋值运算符 *)
  [@@deriving show]

(* 标识符 *)
type identifier = Identifier of string [@@deriving show]

(* 类型 *)
and typ =
  Unit
  | Type of string
  | FnType of {
      params: typ list;
      return: typ;
    }
  [@@deriving show]

(* 表达式 *)
type expr =
  Unit
  | Integer of int
  | Float of float
  | String of string
  | Identifier of identifier
  | UnaryExpr of unaryop * expr
  | BinExpr of expr * binop * expr
  | CallExpr of identifier * expr list

  [@@deriving show]

(* 语句 *)
and statement =
  DefStmt of {
    qualifier: [`Var | `Const];
    lhs: identifier;
    rhs: expr;
  }
  | AssignStmt of {
    lhs: identifier;
    rhs: expr;
  }
  | WhileStmt of {
      condition: expr;
      statements: statement list;
    }
  | FnStmt of {
      ident: identifier;
      params: (identifier * typ) list;
      return: typ;
      statements: statement list;
    }
  | RetStmt of expr
  | IfStmt of {
      guard: expr;
      then_branch: statement list;
      else_branch: statement list;
    }
  [@@deriving show]

(* 整个程序 *)
type prog = Prog of statement list [@@deriving show]
