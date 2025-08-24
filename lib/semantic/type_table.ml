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

let add table alias ty : unit =
  let item = { alias; ty } in
  if List.find_opt (fun map -> map.alias = alias) table.maps = None
    then table.maps <- (item :: table.maps)
    else raise (TypeRedefined (Printf.sprintf "type %s is redefined" (show_typ alias)))

let rec find table alias =
  let map = List.find (fun map -> map.alias = alias) table.maps in
  match map.ty with
    | (Type _) as t -> find table t
    | t -> t

let rec find_opt table alias : typ option =
  let map = List.find_opt (fun map -> map.alias = alias) table.maps in
  match map with
  | None -> None
  | Some { alias = _; ty = ty } -> (match ty with
    | (Type _) as t -> find_opt table t
    | t -> Some t)

let print table =
  table |> show_type_table |> print_endline
