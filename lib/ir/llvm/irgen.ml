open Llvm
open Frontend.Ast
open Semantic

exception Unimplemented
exception Unreachable
exception No_parent_scope

(* LLVM基本环境 *)
let global_context = global_context ()
let the_module = create_module global_context "manbo ir generator"
let builder = builder global_context

(* 生成IR时依赖的上下文 *)
let fn_stack = Stack.create () (* 函数栈 *)
let entry_fn_name = "__manbo_entry"

(* 内置类型 *)
let builtin_types = ["Integer"; "Float"; "String"]

(* 作用域：从llvm全局作用域开始，自顶向下的树形结构 *)
module Scope = struct
  (* 存储变量名到llvalue的映射 *)
  module Id_llvalue_table = struct
    type t = { parent : t option; current : (identifier, llvalue) Hashtbl.t }

    let create () = { parent = None; current = Hashtbl.create 16 }
    let add tbl id v = Hashtbl.add tbl.current id v

    (* 返回当前作用域下的一个新作用域，对该新作用域的修改不会影响到父作用域 *)
    let enter_scope tbl =
      let new_scope = { parent = Some tbl; current = Hashtbl.create 16 } in
      new_scope

    let is_top_scope tbl = tbl.parent = None

    let rec find tbl id =
      try Hashtbl.find tbl.current id with
        | Not_found -> if is_top_scope tbl then raise Not_found else find (Option.get tbl.parent) id

    let rec find_opt tbl id =
      let curr = Hashtbl.find_opt tbl.current id in
      match curr with
      | Some _ as x -> x
      | None -> (
          match tbl.parent with Some parent -> find_opt parent id | None -> None)
  end

  type t = {
    parent: t option;

    id_llvalue_table: Id_llvalue_table.t;

    tytbl: Type_table.type_table;
  }

  let init tytbl = { parent = None; id_llvalue_table = Id_llvalue_table.create (); tytbl }
  
  (* 返回当前作用域下的一个新作用域，对该新作用域的修改不会影响到父作用域 *)
  let enter_scope scope = {
    parent = Some scope;
    id_llvalue_table = Id_llvalue_table.enter_scope scope.id_llvalue_table;
    tytbl = scope.tytbl;
  }

  let exit_scope scope =
    if scope.parent = None then raise No_parent_scope else scope.parent |> Option.get
  
  let is_top_scope scope = scope.parent = None

  let rec top_scope scope =
    if is_top_scope scope
      then scope
      else top_scope (Option.get scope.parent)

  let parent_scope scope = scope.parent


  let add_id_llvalue_map scope id llvalue = Id_llvalue_table.add scope.id_llvalue_table id llvalue

  let id_to_llvalue scope id = Id_llvalue_table.find scope.id_llvalue_table id

  let id_to_llvalue_opt scope id = Id_llvalue_table.find_opt scope.id_llvalue_table id

  let find_type scope ty = Type_table.find scope.tytbl ty

  let find_type_opt scope ty = Type_table.find_opt scope.tytbl ty
end

(* 将manbo中的类型映射到llvm的类型 *)
let rec map_to_llvm_type ty scope =
  match ty with
  | UnitType -> struct_type global_context (Array.of_list [])
  | Type alias as t ->
      (* XXX: 考虑是否将内置类型的判断放入Scope模块中 *)
      if List.find_opt (fun ty_name -> ty_name = alias) builtin_types = None
        then (* 不是内置类型，要找到具体的类型才行 *)
          let actual_ty = Scope.find_type scope t in
          map_to_llvm_type actual_ty scope
        else (* 是内置类型，直接转成指针，后续运行时会在堆上分配 *)
          pointer_type global_context
  | TupleType tys ->
      let llvm_tys = List.map (fun ty -> map_to_llvm_type ty scope) tys in
      struct_type global_context (Array.of_list llvm_tys)
  | FnType f ->
      let ret_llvm_ty = map_to_llvm_type f.return scope in
      let param_llvm_tys =
        List.map (fun ty -> map_to_llvm_type ty scope) f.params
      in
      function_type ret_llvm_ty (Array.of_list param_llvm_tys)
  | StructType s ->
      let field_llvm_tys =
        List.map (fun (_, ty) -> map_to_llvm_type ty scope) s.fields
        |> Array.of_list
      in
      struct_type global_context field_llvm_tys

let ident_to_str (id : identifier) = match id with Identifier s -> s

(* 生成表达式IR *)
let rec gen_expression expr scope =
  match expr with
  (* TODO: 将空元组类型合并到元组类型中 *)
  | Unit -> const_struct global_context (Array.of_list [])
  | Integer i -> gen_integer i
  | Float f -> gen_float f
  | String s -> gen_string s
  | Tuple _ as t -> gen_tuple t scope
  | TupleIndexing (e, i) -> gen_tuple_indexing e i scope
  | Identifier id -> gen_identifier id scope
  | UnaryExpr (uop, e) -> gen_unary_expression uop e scope
  | BinExpr (l, op, r) -> gen_bin_expression l op r scope
  | CallExpr (id, params) -> gen_call_expression id params scope

and gen_integer i =
  let open Llvm_irgen_builtins in
  let v = build_call
    (Integer.__manbo_integer_alloc_type global_context)
    (lookup_function "__manbo_integer_alloc" the_module |> Option.get)
    [|const_int (i64_type global_context) i|] "" builder
  in
  v

and gen_float f =
  let open Llvm_irgen_builtins in
  let v = build_call
    (Float.__manbo_float_alloc_type global_context)
    (lookup_function "__manbo_float_alloc" the_module |> Option.get)
    [|const_float (double_type global_context) f|] "" builder
  in
  v

and gen_string s =
  let v = build_call
    (Llvm_irgen_builtins.String.__manbo_string_alloc_type global_context)
    (lookup_function "__manbo_string_alloc" the_module |> Option.get)
    [|const_string global_context s|] "" builder
  in
  v

and gen_tuple tuple scope =
  let open Llvm_irgen_builtins in
  let exprs = match tuple with
    | Tuple t -> t
    | _ -> raise Unreachable
  in
  let v = build_call
    (Tuple.__manbo_tuple_alloc_type global_context)
    (lookup_function "__manbo_tuple_alloc" the_module |> Option.get)
    (List.map (fun e -> gen_expression e scope) exprs |> Array.of_list)
    ""
    builder
  in
  v

and gen_tuple_indexing e i scope =
  let open Llvm_irgen_builtins in
  let tuple = gen_expression e scope in
  let index = gen_integer i in
  let v = build_call
    (Tuple.__manbo_tuple_indexing_type global_context)
    (lookup_function "__manbo_tuple_indexing" the_module |> Option.get)
    [|tuple; index|] "" builder
  in
  v

and gen_identifier id scope =
  try Scope.id_to_llvalue scope id with
  | Not_found -> (
      match lookup_function (ident_to_str id) the_module with
      | Some f -> f
      | None -> raise Unreachable)

and gen_unary_expression uop e scope =
  let open Llvm_irgen_builtins in
  let e_v = gen_expression e scope in
  match uop with
  | Minus ->
      build_call
        (Unary_op.__manbo_unary_minus_type global_context)
        (lookup_function "__manbo_unary_minus" the_module |> Option.get)
        [|e_v|] "" builder
  | LogicalNot -> raise Unimplemented

and gen_bin_expression l bop r scope =
  let open Helper in

  let l_v = gen_expression l scope in
  let r_v = gen_expression r scope in

  (* 将指针类型中的整数/浮点数类型读取出来 *)
  let l_actual_v = match l_v |> type_of |> classify_type with
    | Pointer -> load_from_ptr l_v builder
    | _ -> l_v
  in
  let r_actual_v = match r_v |> type_of |> classify_type with
    | Pointer -> load_from_ptr r_v builder
    | _ -> r_v
  in

  match (bop, l_actual_v |> type_of |> classify_type) with
  (* 算数运算符 *)
  | Add, Integer -> build_add l_actual_v r_actual_v "" builder
  | Add, Double -> build_fadd l_actual_v r_actual_v "" builder
  | Sub, Integer -> build_sub l_actual_v r_actual_v "" builder
  | Sub, Double -> build_fsub l_actual_v r_actual_v "" builder
  | Mul, Integer -> build_mul l_actual_v r_actual_v "" builder
  | Mul, Double -> build_fmul l_actual_v r_actual_v "" builder
  | Div, Integer -> build_sdiv l_actual_v r_actual_v "" builder
  | Div, Double -> build_fdiv l_actual_v r_actual_v "" builder
  | Mod, Integer -> build_srem l_actual_v r_actual_v "" builder
  | Mod, Double -> build_frem l_actual_v r_actual_v "" builder
  (* 比较运算符 *)
  | Less, Integer -> build_icmp Slt l_actual_v r_actual_v "" builder
  | Less, Double -> build_fcmp Olt l_actual_v r_actual_v "" builder
  | Le, Integer -> build_icmp Sle l_actual_v r_actual_v "" builder
  | Le, Double -> build_fcmp Ole l_actual_v r_actual_v "" builder
  | Equal, Integer -> build_icmp Eq l_actual_v r_actual_v "" builder
  | Equal, Double -> build_fcmp Oeq l_actual_v r_actual_v "" builder
  | Ge, Integer -> build_icmp Sge l_actual_v r_actual_v "" builder
  | Ge, Double -> build_fcmp Oge l_actual_v r_actual_v "" builder
  | Greater, Integer -> build_icmp Sgt l_actual_v r_actual_v "" builder
  | Greater, Double -> build_fcmp Ogt l_actual_v r_actual_v "" builder
  | NotEq, Integer -> build_icmp Ne l_actual_v r_actual_v "" builder
  | NotEq, Double -> build_fcmp One l_actual_v r_actual_v "" builder
  (* TODO: 逻辑运算符 *)
  | _ -> raise Unimplemented

and gen_call_expression id params scope =
  (* 查找函数定义 *)
  let name = ident_to_str id in
  let f = lookup_function name the_module |> Option.get in

  (* 构建函数调用指令 *)
  build_call (type_of f) f
    (List.map (fun e -> gen_expression e scope) params |> Array.of_list)
    "" builder

let rec gen_statements stmts scope =
  match stmts with
    | [] -> ()
    | h :: t -> gen_statement h scope; gen_statements t scope

and gen_statement stmt scope =
  match stmt with
  | DefStmt { qualifier = _; lhs; rhs } -> gen_def_statement lhs rhs scope
  | AssignStmt { lhs; rhs } -> gen_assign_statement lhs rhs scope
  | WhileStmt { condition; statements } -> gen_while_statement condition statements scope
  | RetStmt e -> gen_ret_statement e scope
  | IfStmt { guard = g; then_branch = t; else_branch = e } -> gen_if_statement g t e scope

(* 该函数仅用于在函数作用域内定义变量，在全局作用域定义变量的操作由gen_stmt_item处理*)
and gen_def_statement id e scope =
  let open Scope in
  let v = gen_expression e scope in
  let p = build_alloca (type_of v) (ident_to_str id) builder in
  ignore (build_store v p builder);
  add_id_llvalue_map scope id p;

and gen_assign_statement id e scope =
  let v = gen_expression e scope in
  let var = Scope.id_to_llvalue scope id in
  ignore (build_store v var builder)

and gen_while_statement cond stmts scope =
  let open Scope in
  let open Helper in
  (* 当前所在的函数 *)
  let curr_fn = lookup_function (Stack.top fn_stack) the_module |> Option.get in

  (* 定位到函数末尾块 *)
  let bbs = basic_blocks curr_fn |> Array.to_list |> List.rev in
  let pos = List.nth bbs 0 in
  position_at_end pos builder;

  (* 进入作用域 *)
  let while_scope = enter_scope scope in

  (* 创建基本块 *)
  let cond_bb = append_block global_context "" curr_fn in
  let body_bb = append_block global_context "" curr_fn in
  let end_bb = append_block global_context "" curr_fn in

  (* 构建跳转到while语句起始处的指令 *)
  ignore (build_br cond_bb builder);

  (* 填充cond块 *)
  position_at_end cond_bb builder;
  let cond_v = value_or_load_value (gen_expression cond while_scope) builder in
  let cond_ir = build_icmp Icmp.Eq cond_v (const_int (i1_type global_context) 1) "while.cond" builder in
  ignore (build_cond_br cond_ir body_bb end_bb builder);

  (* 填充body块 *)
  position_at_end body_bb builder;
  gen_statements stmts while_scope;
  ignore (build_br cond_bb builder);

  (* 将builder移动到end块 *)
  position_at_end end_bb builder;

and gen_ret_statement e scope =
  let v = gen_expression e scope in
  ignore (build_ret v builder)

and gen_if_statement guard then_br else_br scope =
  let open Scope in
  
  (* 当前所在的函数 *)
  let curr_fn = lookup_function (Stack.top fn_stack) the_module |> Option.get in

  (* 定位到函数末尾块 *)
  let bbs = basic_blocks curr_fn |> Array.to_list |> List.rev in
  let pos = List.nth bbs 0 in
  position_at_end pos builder;
  
  (* 进入作用域 *)
  let if_scope = enter_scope scope in
    
  (* 创建基本块 *)
  let cond_bb = append_block global_context "" curr_fn in
  let then_bb = append_block global_context "" curr_fn in
  let else_bb = append_block global_context "" curr_fn in
  let end_bb = append_block global_context "" curr_fn in
  
  (* 在函数末尾处，构建跳转到if语句起始处的指令 *)
  ignore (build_br cond_bb builder);
  
  (* 填充cond块 *)
  position_at_end cond_bb builder;
  let guard_v = gen_expression guard if_scope in
  let guard_ir = build_icmp Icmp.Eq guard_v (const_int (i1_type global_context) 1) "while.cond" builder in
  ignore (build_cond_br guard_ir then_bb else_bb builder);
        
  (* 填充then块 *)
  position_at_end then_bb builder;
  gen_statements then_br if_scope;
  ignore (build_br end_bb builder);

  (* 填充else块 *)
  position_at_end else_bb builder;
  gen_statements else_br if_scope;
  ignore (build_br end_bb builder);

  (* 将builder移动到end块 *)
  position_at_end end_bb builder

let rec gen_items items scope =
  match items with
  | [] -> ()
  | h :: t -> gen_item h scope; gen_items t scope

and gen_item item scope =
  match item with
  | StmtItem stmt ->
      let top_fn_scope = Scope.enter_scope scope in
      gen_stmt_item stmt top_fn_scope
  | FnItem f -> gen_fn_item f scope
  | StructItem _ -> ()

(* 调用该函数时，scope是顶级函数作用域 *)
and gen_stmt_item stmt scope =
  match stmt with
    | DefStmt s ->
      (* 说明此时用户在源代码的顶级作用域定义了变量，则该变量在
         编译器中应该定义在顶级函数作用域的父作用域——顶级作用域 *)
      let top_scope = Scope.top_scope scope in
      Scope.add_id_llvalue_map top_scope s.lhs (gen_expression s.rhs top_scope)
    | stmt -> gen_statement stmt scope

(* build_env已经帮我们生成函数的框架了，我们只需在函数里添加语句 *)
and gen_fn_item f scope =
  let open Scope in

  (* 将当前处理的函数push进栈 *)
  Stack.push (f.ident |> ident_to_str) fn_stack;

  (* 进入作用域 *)
  let fn_scope = enter_scope scope in

  (* 设置builder指针 *)
  let fn_name = ident_to_str f.ident in
  let fn = lookup_function fn_name the_module |> Option.get in
  let bbs = basic_blocks fn |> Array.to_list |> List.rev in
  let pos = List.nth bbs 0 in
  position_at_end pos builder;

  (* 翻译语句 *)
  gen_statements f.statements fn_scope;

  (* 将当前函数出栈 *)
  ignore (Stack.pop fn_stack)

(* 生成结构体信息的步骤在build_env中进行，先于整个程序的翻译 *)
let gen_struct_item = None

let rec build_env items scope =
  match items with
    | [] -> ()
    | h :: t -> (match h with
      | StructItem s -> build_struct_type_env s scope
      | FnItem f -> build_fn_type_env f scope
      | _ -> ());
      build_env t scope
    
and build_struct_type_env s scope =
  let fields = s.fields in
  let field_llvm_tys = List.map (fun (_, ty) -> map_to_llvm_type ty scope) fields in
  let struct_name = ident_to_str s.ident in
  let struct_llvm_ty = named_struct_type global_context struct_name in
  struct_set_body struct_llvm_ty (Array.of_list field_llvm_tys) false

and build_fn_type_env f scope =
  let { ident = id; params; return = ret; statements = _ } = f in
  let param_llvm_tys = List.map (fun (_, ty) -> map_to_llvm_type ty scope) params in
  let ret_llvm_ty = map_to_llvm_type ret scope in
  let fn_name = ident_to_str id in
  let fn_llvm_ty = function_type ret_llvm_ty (Array.of_list param_llvm_tys) in 
  ignore (define_function fn_name fn_llvm_ty the_module)

let build_builtin_env llctx llmod =
  let open Llvm_irgen_builtins in
  ignore (Integer.__manbo_integer_alloc_decl llctx llmod);
  ignore (Float.__manbo_float_alloc_decl llctx llmod);
  ignore (String.__manbo_string_alloc_decl llctx llmod);
  ignore (Tuple.__manbo_tuple_alloc_decl llctx llmod);
  ignore (Tuple.__manbo_tuple_indexing_decl llctx llmod);
  ignore (Unary_op.__manbo_unary_minus_decl llctx llmod)

let rec gen_prog prog tytbl =
  (* 初始化上下文 *)
  let scope = Scope.init tytbl in
  
  (* 构建内置函数环境 *)
  build_builtin_env global_context the_module;
  
  (* 构建类型、函数环境 *)
  build_env prog scope;

  (* 生成顶层函数 *)
  let entry_fn = gen_top_fn () in

  (* 将builder指向顶层函数的入口块 *)
  let entry_bb = entry_block entry_fn in
  position_at_end entry_bb builder; 
  
  (* 生成IR。这里传入的上下文是顶层上下文，对应llvm全局作用域 *)
  gen_items prog scope;

and gen_top_fn () =
  let entry_fn_ty = function_type (void_type global_context) [||] in
  let entry_fn = define_function entry_fn_name entry_fn_ty the_module in
  Stack.push entry_fn_name fn_stack;
  entry_fn

(* 调试用：打印生成的IR *)
let dump () = dump_module the_module
