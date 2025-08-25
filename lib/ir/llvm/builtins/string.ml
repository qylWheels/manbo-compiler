(* 生成String类型的内置方法的定义 *)

open Llvm

(* 创建 *)
let __manbo_string_alloc_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx|])
let __manbo_string_alloc_decl llctx llmod =
  declare_function "__manbo_string_alloc" (__manbo_string_alloc_type llctx) llmod

(* 运算 *)
