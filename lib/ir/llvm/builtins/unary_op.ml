(* 一元运算符方法的定义 *)

open Llvm

(* 负号 *)
let __manbo_unary_minus_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx|])
let __manbo_unary_minus_decl llctx llmod =
  declare_function "__manbo_unary_minus" (__manbo_unary_minus_type llctx) llmod
