(* 生成Float类型的内置方法的定义 *)

open Llvm

(* 创建 *)
let __manbo_float_alloc_type llctx =
  function_type (pointer_type llctx) ([|double_type llctx|])
let __manbo_float_alloc_decl llctx llmod =
  declare_function "__manbo_float_alloc" (__manbo_float_alloc_type llctx) llmod

(* 运算 *)
