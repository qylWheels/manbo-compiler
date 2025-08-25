(* 生成Integer类型的内置方法的定义 *)

open Llvm

(* 创建 *)
let __manbo_integer_alloc_type llctx =
  function_type (pointer_type llctx) ([|i64_type llctx|])
let __manbo_integer_alloc_decl llctx llmod =
  declare_function "__manbo_integer_alloc" (__manbo_integer_alloc_type llctx) llmod

(* 运算 *)
