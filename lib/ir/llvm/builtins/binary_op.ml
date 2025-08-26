(* 二元运算符方法的定义 *)

open Llvm

(* 加号 *)
let __manbo_binary_add_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_add_decl llctx llmod =
  declare_function "__manbo_binary_add" (__manbo_binary_add_type llctx) llmod

(* 减号 *)
let __manbo_binary_sub_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_sub_decl llctx llmod =
  declare_function "__manbo_binary_sub" (__manbo_binary_sub_type llctx) llmod

(* 乘号 *)
let __manbo_binary_mul_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_mul_decl llctx llmod =
  declare_function "__manbo_binary_mul" (__manbo_binary_mul_type llctx) llmod

(* 除号 *)
let __manbo_binary_div_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_div_decl llctx llmod =
  declare_function "__manbo_binary_div" (__manbo_binary_div_type llctx) llmod

(* 取余号 *)
let __manbo_binary_rem_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_rem_decl llctx llmod =
  declare_function "__manbo_binary_rem" (__manbo_binary_rem_type llctx) llmod

(* 小于号 *)
let __manbo_binary_less_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_less_decl llctx llmod =
  declare_function "__manbo_binary_less" (__manbo_binary_less_type llctx) llmod

(* 小于等于号 *)
let __manbo_binary_le_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_le_decl llctx llmod =
  declare_function "__manbo_binary_le" (__manbo_binary_le_type llctx) llmod

(* 等于号 *)
let __manbo_binary_equal_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_equal_decl llctx llmod =
  declare_function "__manbo_binary_equal" (__manbo_binary_equal_type llctx) llmod

(* 大于等于号 *)
let __manbo_binary_ge_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_ge_decl llctx llmod =
  declare_function "__manbo_binary_ge" (__manbo_binary_ge_type llctx) llmod

(* 大于号 *)
let __manbo_binary_greater_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_greater_decl llctx llmod =
  declare_function "__manbo_binary_greater" (__manbo_binary_greater_type llctx) llmod

(* 不等号 *)
let __manbo_binary_notequal_type llctx =
  function_type (pointer_type llctx) ([|pointer_type llctx; pointer_type llctx|])
let __manbo_binary_notequal_decl llctx llmod =
  declare_function "__manbo_binary_notequal" (__manbo_binary_notequal_type llctx) llmod

(* 逻辑与号 *)

(* 逻辑或号 *)
