open Llvm
open Frontend.Ast
open Semantic.Check

exception Unimplemented
exception Unreachable

(* 基本环境 *)
let global_context = global_context ()
let the_module = create_module global_context "manbo ir generator"
let builder = builder global_context

(* 类型名称生成器，用于元组类型的生成 *)
module TypeNameGenerator = struct
  type counter = {
    mutable i: int;
  }
  let counter = { i = 0 }

  let new_name () =
    let name = Printf.sprintf "type_%d" counter.i in
    counter.i <- counter.i + 1;
    name
end

(* 变量名称生成器 *)
module VarNameGenerator = struct
  type counter = {
    mutable i: int;
  }
  let counter = { i = 0 }

  let new_name () = 
    let name = Printf.sprintf "var_%d" counter.i in
    counter.i <- counter.i + 1;
    name
end

(* 变量环境，用于存储变量名到llvalue的映射 *)
module VarContext = struct
  type ctx = {
    parent: ctx option;
    current: (identifier, llvalue) Hashtbl.t;
  }
  
  let create parent = {
    parent;
    current = Hashtbl.create 16;
  }

  let add ctx id v = Hashtbl.add ctx.current id v

  let rec find_opt ctx id =
    let curr = Hashtbl.find_opt ctx.current id in
    match curr with
      | (Some _) as x -> x
      | None -> (match ctx.parent with
        | Some parent -> find_opt parent id
        | None -> None)
end

(* 函数环境，用于存储函数名到lltype的映射 *)
module FnContext = struct
  type ctx = {
    current: (identifier, llvalue) Hashtbl.t;
  }
  
  let create () = {
    current = Hashtbl.create 16;
  }

  let add ctx id v = Hashtbl.add ctx.current id v

  let find_opt ctx id =
    let curr = Hashtbl.find_opt ctx.current id in
    match curr with
      | (Some _) as x -> x
      | None -> None
end

(* 类型环境，用于存储类型名到lltype的映射 *)
module TypeContext = struct
  
end

(* 将manbo中的类型映射到llvm的类型 *)
let rec map_to_llvm_type ty tytbl = match ty with
  | UnitType -> struct_type global_context (Array.of_list [])
  | (Type _) as t ->
      let actual_ty = Semantic.Type_table.find_opt tytbl t in
      map_to_llvm_type (Option.get actual_ty) tytbl
  | TupleType tys ->
      let llvm_tys = List.map (fun ty -> map_to_llvm_type ty tytbl) tys in
      struct_type global_context (Array.of_list llvm_tys)
  | FnType f ->
      let ret_llvm_ty = map_to_llvm_type f.return tytbl in
      let param_llvm_tys = List.map (fun ty -> map_to_llvm_type ty tytbl) f.params in
      function_type ret_llvm_ty (Array.of_list param_llvm_tys)
  | StructType s ->
      let field_manbo_tys = List.map (fun (_, ty) -> ty) s.fields in
      let field_llvm_tys =
        (List.map (fun ty -> map_to_llvm_type ty tytbl) field_manbo_tys)
        |> Array.of_list
      in
      struct_type global_context field_llvm_tys

(* 生成表达式IR *)
let rec gen_expression expr vsymtbl fsymtbl tytbl vctx fctx tctx = match expr with
  (* TODO: 将空元组类型合并到元组类型中 *)
  | Unit -> const_struct global_context (Array.of_list [])
  | Integer i -> gen_integer i
  | Float f -> gen_float f
  | String s -> gen_string s
  | (Tuple _) as t -> gen_tuple t vsymtbl fsymtbl tytbl vctx fctx tctx
  | TupleIndexing (e, i) -> gen_tuple_indexing e i vsymtbl fsymtbl tytbl vctx fctx tctx
  | Identifier id -> gen_identifier id vctx fctx
  | UnaryExpr (uop, e) -> gen_unary_expression uop e vsymtbl fsymtbl tytbl vctx fctx tctx
  | BinExpr (l, op, r) -> gen_bin_expression l op r vsymtbl fsymtbl tytbl vctx fctx tctx
  | CallExpr (id, params) -> gen_call_expression id params vsymtbl fsymtbl tytbl vctx fctx tctx

and gen_integer i = const_int (i64_type global_context) i

and gen_float f = const_float (double_type global_context) f

and gen_string s = const_string global_context s

and gen_tuple tuple vsymtbl fsymtbl tytbl vctx fctx tctx =
  (* 确定元组的llvm类型 *)
  let manbo_ty = check_expression tuple vsymtbl fsymtbl tytbl in
  let llvm_ty = map_to_llvm_type manbo_ty tytbl in

  (* 在栈上分配元组内存空间 *)
  let name = VarNameGenerator.new_name () in
  let ptr = build_alloca llvm_ty name builder in

  (* 存储元组内容 *)
  let elems = match tuple with
    | Tuple t -> List.map (fun expr -> gen_expression expr vsymtbl fsymtbl tytbl vctx fctx tctx) t
    | _ -> raise Unreachable
  in
  let rec store elems ptr i = match elems with
    | [] -> ()
    | h :: t ->
        let field_ptr = build_struct_gep llvm_ty ptr i (VarNameGenerator.new_name ()) builder in
        let _ = build_store h field_ptr builder in
        store t ptr (i + 1)
  in
  let _ = store elems ptr 0 in
  build_load llvm_ty ptr (VarNameGenerator.new_name ()) builder

and gen_tuple_indexing e i vsymtbl fsymtbl tytbl vctx fctx tctx =
  (* 对元组表达式求值 *)
  let t = gen_expression e vsymtbl fsymtbl tytbl vctx fctx tctx in

  (* 提取指定下标的内容 *)
  build_extractvalue t i (VarNameGenerator.new_name ()) builder

and gen_identifier id vctx fctx =
  match VarContext.find_opt vctx id with
    | Some v -> v
    | None -> (match FnContext.find_opt fctx id with
      | Some v -> v
      | None -> raise Unreachable)

and gen_unary_expression uop e vsymtbl fsymtbl tytbl vctx fctx tctx =
  let e_ir = gen_expression e vsymtbl fsymtbl tytbl vctx fctx tctx in
  match uop with
    | Minus ->
        (match classify_value e_ir with
          | ConstantInt -> build_neg e_ir (VarNameGenerator.new_name ()) builder
          | ConstantFP -> build_fneg e_ir (VarNameGenerator.new_name ()) builder
          | _ -> raise Unreachable)
    | LogicalNot -> raise Unimplemented

and gen_bin_expression l bop r vsymtbl fsymtbl tytbl vctx fctx tctx =
  let l_v = gen_expression l vsymtbl fsymtbl tytbl vctx fctx tctx in
  let r_v = gen_expression r vsymtbl fsymtbl tytbl vctx fctx tctx in
  match (bop, classify_value l_v) with
    (* 算数运算符 *)
    | (Add, ConstantInt) -> build_add l_v r_v "" builder
    | (Add, ConstantFP) -> build_fadd l_v r_v "" builder
    | (Sub, ConstantInt) -> build_sub l_v r_v "" builder
    | (Sub, ConstantFP) -> build_fsub l_v r_v "" builder
    | (Mul, ConstantInt) -> build_mul l_v r_v "" builder
    | (Mul, ConstantFP) -> build_fmul l_v r_v "" builder
    | (Div, ConstantInt) -> build_sdiv l_v r_v "" builder
    | (Div, ConstantFP) -> build_fdiv l_v r_v "" builder
    | (Mod, ConstantInt) -> build_srem l_v r_v "" builder
    | (Mod, ConstantFP) -> build_frem l_v r_v "" builder

    (* 比较运算符 *)
    | (Less, ConstantInt) -> build_icmp Slt l_v r_v "" builder
    | (Less, ConstantFP) -> build_fcmp Olt l_v r_v "" builder
    | (Le, ConstantInt) -> build_icmp Sle l_v r_v "" builder
    | (Le, ConstantFP) -> build_fcmp Ole l_v r_v "" builder
    | (Equal, ConstantInt) -> build_icmp Eq l_v r_v "" builder
    | (Equal, ConstantFP) -> build_fcmp Oeq l_v r_v "" builder
    | (Ge, ConstantInt) -> build_icmp Sge l_v r_v "" builder
    | (Ge, ConstantFP) -> build_fcmp Oge l_v r_v "" builder
    | (Greater, ConstantInt) -> build_icmp Sgt l_v r_v "" builder
    | (Greater, ConstantFP) -> build_fcmp Ogt l_v r_v "" builder
    | (NotEq, ConstantInt) -> build_icmp Ne l_v r_v "" builder
    | (NotEq, ConstantFP) -> build_fcmp One l_v r_v "" builder

    (* TODO: 逻辑运算符 *)
    | _ -> raise Unimplemented

and gen_call_expression id params vsymtbl fsymtbl tytbl vctx fctx tctx =
  (* 在fctx中找函数定义 *)
  let name = match id with Identifier s -> s in
  let f = lookup_function name the_module |> Option.get in
  build_call
    (type_of f)
    f
    (List.map
      (fun e -> gen_expression e vsymtbl fsymtbl tytbl vctx fctx tctx) params
    |> Array.of_list)
    ""
    builder

(* 调试用：打印生成的IR *)
let dump () = dump_module the_module
