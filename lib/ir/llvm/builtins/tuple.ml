(* 元组类型的内置方法的定义 *)

open Llvm

(* 创建 *)
let __manbo_tuple_alloc_type llctx =
  var_arg_function_type (pointer_type llctx) ([|pointer_type llctx|])
let __manbo_tuple_alloc_decl llctx llmod =
  declare_function "__manbo_tuple_alloc" (__manbo_tuple_alloc_type llctx) llmod

(* 索引 *)
let __manbo_tuple_indexing_type llctx =
  function_type (pointer_type llctx) [|pointer_type llctx; pointer_type llctx|]
let __manbo_tuple_indexing_decl llctx llmod =
  declare_function "__manbo_tuple_indexing" (__manbo_tuple_indexing_type llctx) llmod
