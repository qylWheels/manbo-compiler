open Frontend.Ast
open Sym_table

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
  | Unit, Unit -> true
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
let rec check_expression (expr : expr) (vsymtbl : sym_table)
    (fsymtbl : sym_table) : typ =
  match expr with
  | Unit -> Unit
  | Integer _ -> Type "Integer"
  | Float _ -> Type "Float"
  | String _ -> Type "String"
  | Tuple e_list -> check_tuple_expression e_list vsymtbl fsymtbl
  | Identifier id -> check_id_expression id vsymtbl fsymtbl
  | UnaryExpr (op, e) -> check_unary_expression op e vsymtbl fsymtbl
  | BinExpr (l, op, r) -> check_bin_expression l op r vsymtbl fsymtbl
  | CallExpr (id, params) -> check_call_expression id params vsymtbl fsymtbl

and check_tuple_expression e_list vsymtbl fsymtbl : typ =
  let e_types = List.map (fun e -> check_expression e vsymtbl fsymtbl) e_list in
  TupleType e_types

(* 推断标识符id的类型，先在变量符号表中找，再在函数符号表中找 *)
and check_id_expression id vsymtbl fsymtbl : typ =
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

and check_unary_expression op expr vsymtbl fsymtbl : typ =
  let e_ty = check_expression expr vsymtbl fsymtbl in
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

and check_bin_expression l op r vsymtbl fsymtbl : typ =
  let l_ty = check_expression l vsymtbl fsymtbl in
  let r_ty = check_expression r vsymtbl fsymtbl in
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
      | Type "Integer", Type "Integer" -> Type "Integer"
      | Type "Float", Type "Float" -> Type "Float"
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

and check_call_expression id call_params vsymtbl fsymtbl : typ =
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
            List.map (fun e -> check_expression e vsymtbl fsymtbl) call_params
          in
          let fn_param_tys = p in
          let result = List.equal type_equal call_param_tys fn_param_tys in
          if result = true then r
          else
            raise (Type_error "Parameter type of function call doesn't match")
      | _ -> raise Unreachable)

(* 检查多个语句 *)
let rec check_statements stmts vsymtbl fsymtbl : unit =
  match stmts with
  | [] -> ()
  | h :: t ->
      check_statement h vsymtbl fsymtbl;
      check_statements t vsymtbl fsymtbl

(* 检查单个语句 *)
and check_statement (stmt : statement) (vsymtbl : sym_table)
    (fsymtbl : sym_table) : unit =
  match stmt with
  | DefStmt { qualifier = q; lhs; rhs } ->
      check_def_statement q lhs rhs vsymtbl fsymtbl
  | AssignStmt { lhs; rhs } -> check_assign_statement lhs rhs vsymtbl fsymtbl
  | WhileStmt { condition = cond; statements = stmts } ->
      check_while_statement cond stmts vsymtbl fsymtbl
  | FnStmt { ident; params; return; statements = stmts } ->
      check_fn_statement ident params return stmts vsymtbl fsymtbl
  | RetStmt e -> check_ret_statement e vsymtbl fsymtbl
  | IfStmt { guard; then_branch = tb; else_branch = eb } ->
      check_if_statement guard tb eb vsymtbl fsymtbl

and check_def_statement q lhs rhs vsymtbl fsymtbl : unit =
  let r_ty = check_expression rhs vsymtbl fsymtbl in
  let _ = add_symbol vsymtbl lhs q r_ty in
  ()

and check_assign_statement lhs rhs vsymtbl fsymtbl : unit =
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
  let r_ty = check_expression rhs vsymtbl fsymtbl in
  if l_ty = r_ty then ()
  else
    raise
      (Type_error
         (Printf.sprintf
            "%s has type %s, while %s has type %s, which doesn't match"
            (show_identifier lhs) (show_typ l_ty) (show_expr rhs)
            (show_typ r_ty)))

and check_while_statement cond stmts vsymtbl fsymtbl : unit =
  (* 条件表达式不在while作用域里 *)
  let c_ty = check_expression cond vsymtbl fsymtbl in
  let _ =
    if c_ty = Type "Bool" then ()
    else
      raise
        (Type_error
           (Printf.sprintf "Expected type Bool, found %s" (show_typ c_ty)))
  in
  let while_scope_vsymtbl = enter_scope vsymtbl in
  check_statements stmts while_scope_vsymtbl fsymtbl

and check_fn_statement ident params return stmts vsymtbl fsymtbl : unit =
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
  else ()

(*
  对return语句的检查比较特殊，因为我们无法实现在fsymtbl中找到return语句的定义，
  也无法从return语句往上查找fsymtbl。因此manbo的实现是维护一个全局变量，该变量
  是一个函数定义栈，当前所处的函数在栈顶，这样同时也解决了闭包定义域的问题
*)
and check_ret_statement e vsymtbl fsymtbl : unit =
  let e_ty = check_expression e vsymtbl fsymtbl in
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

and check_if_statement guard then_branch else_branch vsymtbl fsymtbl : unit =
  let _ =
    match check_expression guard vsymtbl fsymtbl with
    | Type "Bool" -> ()
    | ty ->
        raise
          (Type_error
             (Printf.sprintf "Expect type Bool, found %s" (show_typ ty)))
  in
  let if_scope_symtbl = enter_scope vsymtbl in
  check_statements then_branch if_scope_symtbl fsymtbl;
  check_statements else_branch if_scope_symtbl fsymtbl

(* 检查整个程序，返回变量符号表和函数符号表 *)
let check_prog (prog : prog) : sym_table * sym_table =
  let vsymtbl = Sym_table.create () in
  let fsymtbl = Sym_table.create () in
  let stmts = match prog with Prog stmts -> stmts in
  check_statements stmts vsymtbl fsymtbl;
  (vsymtbl, fsymtbl)
