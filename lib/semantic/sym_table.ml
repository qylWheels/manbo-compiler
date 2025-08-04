open Frontend.Ast

type sym_table = {
  parent_scope : sym_table option;
  curr_scope : (identifier, qualifier * typ) Hashtbl.t;
  mutable child_scopes : sym_table list option;
}

(* 创建一个新的、独立的符号表 *)
let create () : sym_table =
  let new_sym_table =
    { parent_scope = None; curr_scope = Hashtbl.create 16; child_scopes = None }
  in
  new_sym_table

(* 在当前作用域下，开启一个新的作用域，返回这个新的作用域 *)
let enter_scope (sym_table : sym_table) : sym_table =
  let new_scope =
    {
      parent_scope = Some sym_table;
      curr_scope = Hashtbl.create 16;
      child_scopes = None;
    }
  in
  let new_child_scopes =
    match sym_table.child_scopes with
    | None -> Some [ new_scope ]
    | Some l -> Some (new_scope :: l)
  in
  sym_table.child_scopes <- new_child_scopes;
  new_scope

(* 退出当前作用域，返回父作用域 *)
let exit_scope (sym_table : sym_table) : sym_table option =
  sym_table.parent_scope

(* 在当前作用域中添加标识符，若当前作用域中已有相同的标识符，则新标识符会覆盖旧标识符信息 *)
let add_symbol (sym_table : sym_table) (sym : identifier) (q : qualifier)
    (typ : typ) : sym_table =
  Hashtbl.add sym_table.curr_scope sym (q, typ);
  sym_table

(* 寻找标识符，若在当前作用域中找不到，则递归地到父作用域中寻找 *)
let rec find_symbol (sym_table : sym_table) (sym : identifier) :
    (qualifier * typ) option =
  let result = Hashtbl.find_opt sym_table.curr_scope sym in
  if result = None then
    match sym_table.parent_scope with
    | Some scope -> find_symbol scope sym
    | None -> None
  else result

(* 打印当前作用域信息，调试用 *)
let print_curr_scope symtable =
  let curr_scope = symtable.curr_scope in
  Hashtbl.iter
    (fun k (q, t) ->
      Printf.printf "%s -> %s %s\n" (show_identifier k) (show_qualifier q)
        (show_typ t))
    curr_scope

(* 打印当前作用域及所有子作用域的信息，调试用 *)
let rec print symtable layer =
  Printf.printf "[layer %d]\n" layer;
  print_curr_scope symtable;
  print_endline "";
  List.iter (fun scope -> print scope (layer + 1)) (Option.value symtable.child_scopes ~default:[])
