open Frontend.Ast
open List

exception TypeRedefined of string

type type_map = {
  alias: typ;
  ty: typ;
}
[@@deriving show]

and type_table = {
  mutable maps: type_map list;
}
[@@deriving show]

let create () = {
  maps = [];
}

(* 内置类型 *)
let builtin_types = ["Integer"; "Float"; "String"]

let add table alias ty : unit =
  let item = { alias; ty } in
  if List.find_opt (fun map -> map.alias = alias) table.maps = None
    then table.maps <- (item :: table.maps)
    else raise (TypeRedefined (Printf.sprintf "type %s is redefined" (show_typ alias)))

let rec find table alias =
  let map = List.find (fun map -> map.alias = alias) table.maps in
  match map.ty with
    | (Type name) as t -> 
        if List.for_all (fun ty -> ty <> name) builtin_types
          then find table t (* 不是内置类型，继续递归检查 *)
          else t (* 已经是内置类型，停止递归检查 *)
    | t -> t

let rec find_opt table alias : typ option =
  let map = List.find_opt (fun map -> map.alias = alias) table.maps in
  match map with
  | None -> None
  | Some { alias = _; ty = ty } -> (match ty with
    | (Type name) as t ->
        if List.for_all (fun ty -> ty <> name) builtin_types
          then find_opt table t (* 不是内置类型，继续递归检查 *)
          else Some t (* 已经是内置类型，停止递归检查 *)
    | t -> Some t)

let print table =
  table |> show_type_table |> print_endline
