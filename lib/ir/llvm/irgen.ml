open Llvm
open Frontend.Ast
open Semantic.Check
open Semantic.Sym_table
open Semantic.Type_table

exception Unimplemented
exception Unreachable

(* LLVM基本环境 *)
let global_context = global_context ()
let the_module = create_module global_context "manbo ir generator"
let builder = builder global_context

(* 生成IR时依赖的上下文 *)
let fn_stack = Stack.create () (* 函数栈 *)

(* 类型名称生成器，用于元组类型的生成 *)
module TypeNameGenerator = struct
  type counter = { mutable i : int }

  let counter = { i = 0 }

  let new_name () =
    let name = Printf.sprintf "type_%d" counter.i in
    counter.i <- counter.i + 1;
    name
end

(* 变量名称生成器 *)
module VarNameGenerator = struct
  type counter = { mutable i : int }

  let counter = { i = 0 }

  let new_name () =
    let name = Printf.sprintf "var_%d" counter.i in
    counter.i <- counter.i + 1;
    name
end

(* 变量环境，用于存储变量名到llvalue的映射 *)
module Var_Val_Table = struct
  type t = { parent : t option; current : (identifier, llvalue) Hashtbl.t }

  exception No_parent

  let create parent = { parent; current = Hashtbl.create 16 }
  let add ctx id v = Hashtbl.add ctx.current id v

  let enter_scope ctx =
    let new_scope = { parent = Some ctx; current = Hashtbl.create 16 } in
    new_scope

  let exit_scope ctx = match ctx.parent with
    | Some x -> x
    | None -> raise No_parent

  let rec find_opt ctx id =
    let curr = Hashtbl.find_opt ctx.current id in
    match curr with
    | Some _ as x -> x
    | None -> (
        match ctx.parent with Some parent -> find_opt parent id | None -> None)
end

(* 函数环境，用于存储函数名到lltype的映射 *)
module Fn_Val_Table = struct
  type t = { current : (identifier, llvalue) Hashtbl.t }

  let create () = { current = Hashtbl.create 16 }
  let add ctx id v = Hashtbl.add ctx.current id v

  let find_opt ctx id =
    let curr = Hashtbl.find_opt ctx.current id in
    match curr with Some _ as x -> x | None -> None
end

(* 上下文 *)
type context = {
  vsymtbl : sym_table;
  fsymtbl : sym_table;
  tytbl : type_table;
  vvaltbl : Var_Val_Table.t;
  fvaltbl : Fn_Val_Table.t;
}

(* 将manbo中的类型映射到llvm的类型 *)
let rec map_to_llvm_type ty tytbl =
  match ty with
  | UnitType -> struct_type global_context (Array.of_list [])
  | Type _ as t ->
      let actual_ty = Semantic.Type_table.find_opt tytbl t in
      map_to_llvm_type (Option.get actual_ty) tytbl
  | TupleType tys ->
      let llvm_tys = List.map (fun ty -> map_to_llvm_type ty tytbl) tys in
      struct_type global_context (Array.of_list llvm_tys)
  | FnType f ->
      let ret_llvm_ty = map_to_llvm_type f.return tytbl in
      let param_llvm_tys =
        List.map (fun ty -> map_to_llvm_type ty tytbl) f.params
      in
      function_type ret_llvm_ty (Array.of_list param_llvm_tys)
  | StructType s ->
      let field_manbo_tys = List.map (fun (_, ty) -> ty) s.fields in
      let field_llvm_tys =
        List.map (fun ty -> map_to_llvm_type ty tytbl) field_manbo_tys
        |> Array.of_list
      in
      struct_type global_context field_llvm_tys

(* 生成表达式IR *)
let rec gen_expression expr ctx =
  match expr with
  (* TODO: 将空元组类型合并到元组类型中 *)
  | Unit -> const_struct global_context (Array.of_list [])
  | Integer i -> gen_integer i
  | Float f -> gen_float f
  | String s -> gen_string s
  | Tuple _ as t -> gen_tuple t ctx
  | TupleIndexing (e, i) -> gen_tuple_indexing e i ctx
  | Identifier id -> gen_identifier id ctx
  | UnaryExpr (uop, e) -> gen_unary_expression uop e ctx
  | BinExpr (l, op, r) -> gen_bin_expression l op r ctx
  | CallExpr (id, params) -> gen_call_expression id params ctx

and gen_integer i = const_int (i64_type global_context) i
and gen_float f = const_float (double_type global_context) f
and gen_string s = const_string global_context s

and gen_tuple tuple ctx =
  (* 确定元组的llvm类型 *)
  let manbo_ty = check_expression tuple ctx.vsymtbl ctx.fsymtbl ctx.tytbl in
  let llvm_ty = map_to_llvm_type manbo_ty ctx.tytbl in

  (* 在栈上分配元组内存空间 *)
  let name = VarNameGenerator.new_name () in
  let ptr = build_alloca llvm_ty name builder in

  (* 存储元组内容 *)
  let elems =
    match tuple with
    | Tuple t -> List.map (fun expr -> gen_expression expr ctx) t
    | _ -> raise Unreachable
  in
  let rec store elems ptr i =
    match elems with
    | [] -> ()
    | h :: t ->
        let field_ptr =
          build_struct_gep llvm_ty ptr i (VarNameGenerator.new_name ()) builder
        in
        let _ = build_store h field_ptr builder in
        store t ptr (i + 1)
  in
  let _ = store elems ptr 0 in
  build_load llvm_ty ptr (VarNameGenerator.new_name ()) builder

and gen_tuple_indexing e i ctx =
  (* 对元组表达式求值 *)
  let t = gen_expression e ctx in

  (* 提取指定下标的内容 *)
  build_extractvalue t i (VarNameGenerator.new_name ()) builder

and gen_identifier id ctx =
  match Var_Val_Table.find_opt ctx.vvaltbl id with
  | Some v -> v
  | None -> (
      match Fn_Val_Table.find_opt ctx.fvaltbl id with
      | Some v -> v
      | None -> raise Unreachable)

and gen_unary_expression uop e ctx =
  let e_ir = gen_expression e ctx in
  match uop with
  | Minus -> (
      match classify_value e_ir with
      | ConstantInt -> build_neg e_ir (VarNameGenerator.new_name ()) builder
      | ConstantFP -> build_fneg e_ir (VarNameGenerator.new_name ()) builder
      | _ -> raise Unreachable)
  | LogicalNot -> raise Unimplemented

and gen_bin_expression l bop r ctx =
  let l_v = gen_expression l ctx in
  let r_v = gen_expression r ctx in
  match (bop, classify_value l_v) with
  (* 算数运算符 *)
  | Add, ConstantInt -> build_add l_v r_v "" builder
  | Add, ConstantFP -> build_fadd l_v r_v "" builder
  | Sub, ConstantInt -> build_sub l_v r_v "" builder
  | Sub, ConstantFP -> build_fsub l_v r_v "" builder
  | Mul, ConstantInt -> build_mul l_v r_v "" builder
  | Mul, ConstantFP -> build_fmul l_v r_v "" builder
  | Div, ConstantInt -> build_sdiv l_v r_v "" builder
  | Div, ConstantFP -> build_fdiv l_v r_v "" builder
  | Mod, ConstantInt -> build_srem l_v r_v "" builder
  | Mod, ConstantFP -> build_frem l_v r_v "" builder
  (* 比较运算符 *)
  | Less, ConstantInt -> build_icmp Slt l_v r_v "" builder
  | Less, ConstantFP -> build_fcmp Olt l_v r_v "" builder
  | Le, ConstantInt -> build_icmp Sle l_v r_v "" builder
  | Le, ConstantFP -> build_fcmp Ole l_v r_v "" builder
  | Equal, ConstantInt -> build_icmp Eq l_v r_v "" builder
  | Equal, ConstantFP -> build_fcmp Oeq l_v r_v "" builder
  | Ge, ConstantInt -> build_icmp Sge l_v r_v "" builder
  | Ge, ConstantFP -> build_fcmp Oge l_v r_v "" builder
  | Greater, ConstantInt -> build_icmp Sgt l_v r_v "" builder
  | Greater, ConstantFP -> build_fcmp Ogt l_v r_v "" builder
  | NotEq, ConstantInt -> build_icmp Ne l_v r_v "" builder
  | NotEq, ConstantFP -> build_fcmp One l_v r_v "" builder
  (* TODO: 逻辑运算符 *)
  | _ -> raise Unimplemented

and gen_call_expression id params ctx =
  (* 在fctx中找函数定义 *)
  let name = match id with Identifier s -> s in
  let f = lookup_function name the_module |> Option.get in

  (* 构建函数调用指令 *)
  build_call (type_of f) f
    (List.map (fun e -> gen_expression e ctx) params |> Array.of_list)
    "" builder

let rec gen_statements stmts ctx =
  match stmts with
    | [] -> ()
    | h :: t -> gen_statement h ctx; gen_statements t ctx

and gen_statement stmt ctx = match stmt with
  | DefStmt { qualifier = _; lhs; rhs } -> gen_def_statement lhs rhs ctx
  | AssignStmt { lhs; rhs } -> gen_assign_statement lhs rhs ctx
  | WhileStmt { condition; statements } -> gen_while_statement condition statements ctx
  | RetStmt e -> gen_ret_statement e ctx
  | IfStmt { guard = g; then_branch = t; else_branch = e } -> gen_if_statement g t e ctx

and gen_def_statement id e ctx =
  let v = gen_expression e ctx in
  let local_var = build_alloca (type_of v) "" builder in
  ignore (build_store v local_var builder);
  Var_Val_Table.add ctx.vvaltbl id local_var (* 要存储alloca出来的指针，以备重新赋值的时候使用 *)

and gen_assign_statement id e ctx =
  let v = gen_expression e ctx in
  let local_var = Var_Val_Table.find_opt ctx.vvaltbl id |> Option.get in
  ignore (build_store v local_var builder)

and gen_while_statement cond stmts ctx =
  (* 当前所在的函数 *)
  let curr_fn = Stack.top fn_stack in

  (* 进入作用域 *)
  let while_scope = Var_Val_Table.enter_scope ctx.vvaltbl in
  let while_ctx = { ctx with vvaltbl = while_scope } in

  (* 创建基本块 *)
  let cond_bb = append_block global_context "" curr_fn in
  let body_bb = append_block global_context "" curr_fn in
  let end_bb = append_block global_context "" curr_fn in

  (* 构建跳转到while语句起始处的指令 *)
  ignore (build_br cond_bb builder);

  (* 填充cond块 *)
  position_at_end cond_bb builder;
  let cond_v = gen_expression cond while_ctx in
  let cond_ir = build_icmp Icmp.Eq cond_v (const_int (i1_type global_context) 1) "while.cond" builder in
  ignore (build_cond_br cond_ir body_bb end_bb builder);

  (* 填充body块 *)
  position_at_end body_bb builder;
  gen_statements stmts while_ctx;
  ignore (build_br cond_bb builder);

  (* 将builder移动到end块 *)
  position_at_end end_bb builder;

  (* 离开作用域 *)
  ignore (Var_Val_Table.exit_scope while_ctx.vvaltbl)

and gen_ret_statement e ctx =
  let v = gen_expression e ctx in
  ignore (build_ret v builder)

and gen_if_statement guard then_br else_br ctx =
  (* 当前所在的函数 *)
  let curr_fn = Stack.top fn_stack in

  (* 进入作用域 *)
  let if_scope = Var_Val_Table.enter_scope ctx.vvaltbl in
  let if_ctx = { ctx with vvaltbl = if_scope } in

  (* 创建基本块 *)
  let cond_bb = append_block global_context "" curr_fn in
  let then_bb = append_block global_context "" curr_fn in
  let else_bb = append_block global_context "" curr_fn in
  let end_bb = append_block global_context "" curr_fn in

  (* 构建跳转到if语句起始处的指令 *)
  ignore (build_br cond_bb builder);

  (* 填充cond块 *)
  position_at_end cond_bb builder;
  let guard_v = gen_expression guard if_ctx in
  let guard_ir = build_icmp Icmp.Eq guard_v (const_int (i1_type global_context) 1) "while.cond" builder in
  ignore (build_cond_br guard_ir then_bb else_bb builder);

  (* 填充then块 *)
  position_at_end then_bb builder;
  gen_statements then_br if_ctx;
  ignore (build_br end_bb builder);

  (* 填充else块 *)
  position_at_end else_bb builder;
  gen_statements else_br if_ctx;
  ignore (build_br end_bb builder);

  (* 将builder移动到end块 *)
  position_at_end end_bb builder;

  (* 离开作用域 *)
  ignore (Var_Val_Table.exit_scope if_ctx.vvaltbl)

(* let rec gen_items items ctx =

and gen_item item ctx = *)

let rec gen_prog _prog vsymtbl fsymtbl tytbl =
  (* 初始化环境 *)
  let vvaltbl = Var_Val_Table.create None in
  let fvaltbl = Fn_Val_Table.create () in
  let ctx = {
    vsymtbl; fsymtbl; tytbl; vvaltbl; fvaltbl;
  }
  in

  (* 生成顶层函数 *)
  let _ = gen_top_fn ctx in

  raise Unimplemented

and gen_top_fn ctx =
  let top_fn_ty = function_type (void_type global_context) [||] in
  let top_fn = define_function "__manbo_top_function" top_fn_ty the_module in
  let _ = Fn_Val_Table.add ctx.fvaltbl (Identifier "__manbo_top_function") top_fn in
  let _ = Stack.push top_fn fn_stack in
  top_fn

(* 调试用：打印生成的IR *)
let dump () = dump_module the_module
