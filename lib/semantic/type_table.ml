open Frontend.Ast
open List

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
  table.maps <- (item :: table.maps);
  ()

let find_opt table alias : typ option =
  let map = List.find_opt (fun map -> map.alias = alias) table.maps in
  match map with
  | None -> None
  | Some { alias = _; ty = ty } -> Some ty

(* let find_ty_opt table ty : typ option = 
  let map = List.find_opt (fun map -> map.ty = ty) table.maps in
  match map with
  | None -> None
  | Some { alias = _; ty = ty } -> Some ty *)

let print table =
  table |> show_type_table |> print_endline
