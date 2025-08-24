open Frontend.Ast
open Sym_table
open Type_table

exception Type_error of string
exception Undefined_error of string
exception Redefined_error of string
exception Operator_error of string
exception Mutability_error of string
exception Unreachable
exception Unimplemented

(*
  函数定义栈，包含函数名、函数类型、返回语句信息。
  定义函数时将函数入栈，定义结束时将函数出栈，以保证栈顶存放的是当前所在的函数
*)
let active_fn_stack = Stack.create ()

(* 比较两个类型是否相同 *)
let rec type_equal (ty1 : typ) (ty2 : typ) : bool =
  match (ty1, ty2) with
  | UnitType, UnitType -> true
  | Type a, Type b when a = b -> true
  | TupleType a, TupleType b ->
      let rec equal alist blist =
        if List.length alist <> List.length blist
          then false
          else match alist, blist with
            | [], [] -> true
            | (ha :: ta), (hb :: tb) -> type_equal ha hb && equal ta tb
            | _, _ -> raise Unreachable
      in
      equal a b
  | FnType { params = p1; return = r1 }, FnType { params = p2; return = r2 } ->
      let result = List.equal type_equal p1 p2 && type_equal r1 r2 in
      result
  | _, _ -> false

(* 检查单个表达式 *)
let rec check_expression expr vsymtbl fsymtbl tytbl : typ =
  match expr with
  | Unit -> UnitType
  | Integer _ -> Type "Integer"
  | Float _ -> Type "Float"
  | String _ -> Type "String"
  | Tuple e_list -> check_tuple_expression e_list vsymtbl fsymtbl tytbl
  | TupleIndexing (e, i) -> check_tuple_indexing_expression e i vsymtbl fsymtbl tytbl
  | Identifier id -> check_id_expression id vsymtbl fsymtbl tytbl
  | UnaryExpr (op, e) -> check_unary_expression op e vsymtbl fsymtbl tytbl
  | BinExpr (l, op, r) -> check_bin_expression l op r vsymtbl fsymtbl tytbl
  | CallExpr (id, params) -> check_call_expression id params vsymtbl fsymtbl tytbl

and check_tuple_expression e_list vsymtbl fsymtbl tytbl : typ =
  let e_types = List.map (fun e -> check_expression e vsymtbl fsymtbl tytbl) e_list in
  TupleType e_types

and check_tuple_indexing_expression e i vsymtbl fsymtbl tytbl : typ =
  let e_ty = check_expression e vsymtbl fsymtbl tytbl in
  match e_ty with
  | TupleType l ->
      let len = List.length l in
      if i < len then List.nth l i
      else raise (Type_error "Index exceeds tuple length")
  | ty -> raise (Type_error (Printf.sprintf "Expected type Tuple, found %s" (show_typ ty)))

(* 推断标识符id的类型，先在变量符号表中找，再在函数符号表中找 *)
and check_id_expression id vsymtbl fsymtbl _tytbl : typ =
  let v = find_symbol vsymtbl id in
  match v with
  | Some (_, ty) -> ty
  | None -> (
      let f = find_symbol fsymtbl id in
      match f with
      | Some (_, ty) -> ty
      | None ->
          raise
            (Undefined_error
               (Printf.sprintf "Undefined identifier: %s" (show_identifier id)))
      )

and check_unary_expression op expr vsymtbl fsymtbl tytbl : typ =
  let e_ty = check_expression expr vsymtbl fsymtbl tytbl in
  match op with
  | Minus -> (
      match e_ty with
      | Type "Integer" | Type "Float" -> e_ty
      | ty ->
          raise
            (Type_error
               (Printf.sprintf "expect type Integer or Float, found %s"
                  (show_typ ty))))
  | LogicalNot -> failwith "Not implemented yet"

and check_bin_expression l op r vsymtbl fsymtbl tytbl : typ =
  let l_ty = check_expression l vsymtbl fsymtbl tytbl in
  let r_ty = check_expression r vsymtbl fsymtbl tytbl in
  match op with
  | Add | Sub | Mul | Div | Mod -> (
      match (l_ty, r_ty) with
      | Type "Integer", Type "Integer" -> Type "Integer"
      | Type "Float", Type "Float" -> Type "Float"
      | l, r ->
          raise
            (Type_error
               (Printf.sprintf "expect type Integer or Float, found %s and %s"
                  (show_typ l) (show_typ r))))
  | Less | Le | Ge | Greater -> (
      match (l_ty, r_ty) with
      | Type "Integer", Type "Integer" | Type "Float", Type "Float" ->
          Type "Bool"
      | l, r ->
          raise
            (Type_error
               (Printf.sprintf "expect type Integer or Float, found %s and %s"
                  (show_typ l) (show_typ r))))
  | Equal | NotEq -> (
      match (l_ty, r_ty) with
      | Type "Integer", Type "Integer" -> Type "Bool"
      | Type "Float", Type "Float" -> Type "Bool"
      | l, r ->
          raise
            (Type_error
               (Printf.sprintf "expect type Integer or Float, found %s and %s"
                  (show_typ l) (show_typ r))))
  | LogicalAnd | LogicalOr -> failwith "Not implemented yet"
  | _ ->
      raise
        (Operator_error
           (Printf.sprintf "Unexpected binary operator: %s" (show_binop op)))

and check_call_expression id call_params vsymtbl fsymtbl tytbl : typ =
  (* 先在vsymtbl中找函数定义，再在fsymtbl中找，因为vsymtbl一定是“更近的” *)
  let f_ty =
    match find_symbol vsymtbl id with
    | Some (_, ty) -> Some ty
    | None -> (
        match find_symbol fsymtbl id with
        | Some (_, ty) -> Some ty
        | None -> None)
  in
  match f_ty with
  | None ->
      raise
        (Undefined_error
           (Printf.sprintf "Undefined function: %s" (show_identifier id)))
  | Some f_ty -> (
      match f_ty with
      | FnType { params = p; return = r } ->
          let call_param_tys =
            List.map (fun e -> check_expression e vsymtbl fsymtbl tytbl) call_params
          in
          let fn_param_tys = p in
          let result = List.equal type_equal call_param_tys fn_param_tys in
          if result = true then r
          else
            raise (Type_error "Parameter type of function call doesn't match")
      | _ -> raise Unreachable)

(* 检查多个语句 *)
let rec check_statements stmts vsymtbl fsymtbl tytbl : unit =
  match stmts with
  | [] -> ()
  | h :: t ->
      check_statement h vsymtbl fsymtbl tytbl;
      check_statements t vsymtbl fsymtbl tytbl

(* 检查单个语句 *)
and check_statement stmt vsymtbl fsymtbl tytbl : unit =
  match stmt with
  | DefStmt { qualifier = q; lhs; rhs } ->
      check_def_statement q lhs rhs vsymtbl fsymtbl tytbl
  | AssignStmt { lhs; rhs } -> check_assign_statement lhs rhs vsymtbl fsymtbl tytbl
  | WhileStmt { condition = cond; statements = stmts } ->
      check_while_statement cond stmts vsymtbl fsymtbl tytbl
  | RetStmt e -> check_ret_statement e vsymtbl fsymtbl tytbl
  | IfStmt { guard; then_branch = tb; else_branch = eb } ->
      check_if_statement guard tb eb vsymtbl fsymtbl tytbl

and check_def_statement q lhs rhs vsymtbl fsymtbl tytbl : unit =
  let r_ty = check_expression rhs vsymtbl fsymtbl tytbl in
  let _ = add_symbol vsymtbl lhs q r_ty in
  ()

and check_assign_statement lhs rhs vsymtbl fsymtbl tytbl : unit =
  let l_q, l_ty =
    match find_symbol vsymtbl lhs with
    | Some (q, ty) -> (q, ty)
    | None ->
        raise
          (Undefined_error
             (Printf.sprintf "Undefined identifier: %s" (show_identifier lhs)))
  in
  let _ =
    if l_q = Const then
      raise
        (Mutability_error
           (Printf.sprintf "%s is immutable" (show_identifier lhs)))
    else ()
  in
  let r_ty = check_expression rhs vsymtbl fsymtbl tytbl in
  if l_ty = r_ty then ()
  else
    raise
      (Type_error
         (Printf.sprintf
            "%s has type %s, while %s has type %s, which doesn't match"
            (show_identifier lhs) (show_typ l_ty) (show_expr rhs)
            (show_typ r_ty)))

and check_while_statement cond stmts vsymtbl fsymtbl tytbl : unit =
  (* 条件表达式不在while作用域里 *)
  let c_ty = check_expression cond vsymtbl fsymtbl tytbl in
  let _ =
    if c_ty = Type "Bool" then ()
    else
      raise
        (Type_error
           (Printf.sprintf "Expected type Bool, found %s" (show_typ c_ty)))
  in
  let while_scope_vsymtbl = enter_scope vsymtbl in
  check_statements stmts while_scope_vsymtbl fsymtbl tytbl

(* and check_fn_statement ident params return stmts vsymtbl fsymtbl: unit =
  (* 先确定函数的类型并将其加入fsymtbl，以支持递归 *)
  let fn_type =
    FnType { params = List.map (fun (_, ty) -> ty) params; return }
  in
  let _ = add_symbol fsymtbl ident Const fn_type in

  (* 确定函数类型后，将当前函数push入函数定义栈 *)
  let _ = Stack.push (ident, fn_type, []) active_fn_stack in

  (* 再将函数参数加入函数作用域中 *)
  let fn_scope_vsymtbl = enter_scope vsymtbl in
  let rec add_params_into_vsymtbl params =
    match params with
    | [] -> ()
    | (id, ty) :: t ->
        let _ = add_symbol fn_scope_vsymtbl id Const ty in
        add_params_into_vsymtbl t
  in
  let _ = add_params_into_vsymtbl params in

  (* 最后检查函数中的语句 *)
  let _ = check_statements stmts fn_scope_vsymtbl fsymtbl in

  (* 结束函数定义后，pop函数定义，并检查return语句是否为空，若为空，报错 *)
  let fn_id, _, ret_stmts = Stack.pop active_fn_stack in
  if ret_stmts = [] then
    let fn_name = match fn_id with Identifier name -> name in
    raise
      (Type_error (Printf.sprintf "No return statement in function %s" fn_name))
  else () *)

(*
  对return语句的检查比较特殊，因为我们无法实现在fsymtbl中找到return语句的定义，
  也无法从return语句往上查找fsymtbl。因此manbo的实现是维护一个全局变量，该变量
  是一个函数定义栈，当前所处的函数在栈顶，这样同时也解决了闭包定义域的问题
*)
and check_ret_statement e vsymtbl fsymtbl tytbl : unit =
  let e_ty = check_expression e vsymtbl fsymtbl tytbl in
  let fn_id, fn_ty, ret_stmts = Stack.pop active_fn_stack in
  let ret_ty =
    match fn_ty with
    | FnType { params = _; return = ret_ty } -> ret_ty
    | _ -> raise Unreachable
  in
  if type_equal e_ty ret_ty then
    Stack.push (fn_id, fn_ty, RetStmt e :: ret_stmts) active_fn_stack
  else
    raise
      (Type_error
         "Type in return statement and function return type doesn't match")

and check_if_statement guard then_branch else_branch vsymtbl fsymtbl tytbl : unit =
  let _ =
    match check_expression guard vsymtbl fsymtbl tytbl with
    | Type "Bool" -> ()
    | ty ->
        raise
          (Type_error
             (Printf.sprintf "Expect type Bool, found %s" (show_typ ty)))
  in
  let if_scope_symtbl = enter_scope vsymtbl in
  check_statements then_branch if_scope_symtbl fsymtbl tytbl;
  check_statements else_branch if_scope_symtbl fsymtbl tytbl

(* 检查源文件中的所有要素 *)
let rec check_items items vsymtbl fsymtbl tytbl : unit =
  match items with
  | [] -> ()
  | h :: t -> check_item h vsymtbl fsymtbl tytbl; check_items t vsymtbl fsymtbl tytbl

and check_item item vsymtbl fsymtbl tytbl : unit =
  match item with
  | StmtItem s -> check_statement s vsymtbl fsymtbl tytbl
  | FnItem f -> check_fn_item f vsymtbl fsymtbl tytbl
  | StructItem s -> check_struct_item s tytbl

and check_fn_item f vsymtbl fsymtbl tytbl : unit =
  let { ident; params; return = _; statements = stmts } = f in

  (* 检查函数是否已定义 *)
  let fn_ty = match find_symbol fsymtbl ident with
    | None -> raise (Undefined_error (Printf.sprintf "Function \"%s\" is undefined" (show_identifier ident)))
    | Some (_, ty) -> ty
  in

  (* 检查函数的参数类型和返回类型是否已定义 *)
  let (fn_param_tys, fn_ret_ty) = match fn_ty with
    | FnType { params; return } -> (params, return)
    | _ -> raise Unreachable
  in
  let filtered_fn_param_tys = List.filter (* 只有形如Type "xxx"的类型才需要检查 *)
    (fun ty -> match ty with
      | Type _ -> true
      | _ -> false)
    fn_param_tys
  in
  let _ = List.iter
    (fun ty -> match Type_table.find_opt tytbl ty with
      | Some _ -> ()
      | None -> raise (Type_error (Printf.sprintf "Type \"%s\" is undefined" (show_typ ty)))) filtered_fn_param_tys
  in
  let _ = match fn_ret_ty with (* 只有形如Type "xxx"的类型才需要检查 *)
    | Type _ -> (match Type_table.find_opt tytbl fn_ret_ty with
      | Some _ -> ()
      | None -> raise (Type_error (Printf.sprintf "Type \"%s\" is undefined" (show_typ fn_ret_ty))))
    | _ -> ()
  in

  (* 确定函数类型后，将当前函数push入函数定义栈 *)
  let _ = Stack.push (ident, fn_ty, []) active_fn_stack in

  (* 再将函数参数加入函数作用域中 *)
  let fn_scope_vsymtbl = enter_scope vsymtbl in
  let rec add_params_into_vsymtbl params =
    match params with
    | [] -> ()
    | (id, ty) :: t ->
        let _ = add_symbol fn_scope_vsymtbl id Const ty in
        add_params_into_vsymtbl t
  in
  let _ = add_params_into_vsymtbl params in

  (* 最后检查函数中的语句 *)
  let _ = check_statements stmts fn_scope_vsymtbl fsymtbl tytbl in

  (* 结束函数定义后，pop函数定义，并检查return语句是否为空，若为空，报错 *)
  let fn_id, _, ret_stmts = Stack.pop active_fn_stack in
  if ret_stmts = [] then
    let fn_name = match fn_id with Identifier name -> name in
    raise
      (Type_error (Printf.sprintf "No return statement in function %s" fn_name))
  else ()

(* FIXME: struct中不能有同名字段 *)
and check_struct_item s tytbl : unit =
  (* 检查是否有重复字段名 *)
  let tbl = Hashtbl.create 16 in
  let rec check_dup_field l = match l with
    | [] -> ()
    | (id, _) :: t ->
        let has = Hashtbl.find_opt tbl id in
        if has = None
          then
            let _ = Hashtbl.add tbl id () in
            check_dup_field t
          else raise (Redefined_error (Printf.sprintf "field %s is redefined" (show_identifier id)))
  in
  check_dup_field s.fields;

  (* 只有Type "xxx"形式的类型需要检查 *)
  let field_tys = List.map (fun (_, ty) -> ty) s.fields in
  let filtered_field_tys = List.filter
    (fun ty -> match ty with
      | Type _ -> true
      | _ -> false)
    field_tys
  in

  let _ = List.iter
    (fun ty -> match Type_table.find_opt tytbl ty with
      | Some _ -> ()
      | None -> raise (Type_error (Printf.sprintf "Type \"%s\" is undefined" (show_typ ty)))
    )
    filtered_field_tys
  in
  ()

let rec build_env items fsymtbl tytbl : unit =
  let build_single_item item fsymtbl tytbl = match item with
    | FnItem f -> build_fn_item f fsymtbl tytbl
    | StructItem s -> build_struct_item s fsymtbl tytbl
    | _ -> ()
  in
  match items with
  | [] -> ()
  | h :: t -> build_single_item h fsymtbl tytbl; build_env t fsymtbl tytbl

and build_fn_item item fsymtbl _tytbl : unit =
  let { ident; params; return; statements = _ } = item in

  (* 检查是否重定义 *)
  let result = Sym_table.find_symbol fsymtbl ident in
  let _ = match result with
    | None -> ()
    | Some _ -> raise (Redefined_error (Printf.sprintf "Function %s is already defined" (show_identifier ident)))
  in

  (* 将函数加入到fsymtbl中，注意此时不检查函数体中语句的类型 *)
  let fn_type =
    FnType { params = List.map (fun (_, ty) -> ty) params; return }
  in
  let _ = add_symbol fsymtbl ident Const fn_type in
  ()

and build_struct_item item _fsymtbl tytbl : unit =
  let struct_ty = StructType { ident = item.ident; fields = item.fields } in
  let struct_name = match item.ident with
    | Identifier name -> name
  in
  Type_table.add tytbl (Type struct_name) struct_ty

(* 检查整个程序，返回变量符号表、函数符号表和类型环境表 *)
let check_prog (prog : prog) : sym_table * sym_table * type_table  =
  let vsymtbl = Sym_table.create () in
  let fsymtbl = Sym_table.create () in
  let tytbl = Type_table.create () in
  let items = prog in

  (* 第一轮遍历AST，将函数、结构体、枚举类型加入对应的表中，构建类型环境， *)
  (* 以支持递归和先使用后定义 *)
  let _ = build_env items fsymtbl tytbl in
  (* 将Integer、Float、String等默认类型加入tytbl *)
  let _ =
    Type_table.add tytbl (Type "Integer") (Type "Integer");
    Type_table.add tytbl (Type "Float") (Type "Float");
    Type_table.add tytbl (Type "String") (Type "String")
  in

  (* 第二轮遍历AST，对所有项目进行类型检查 *)
  let _ = check_items items vsymtbl fsymtbl tytbl in

  (vsymtbl, fsymtbl, tytbl)
