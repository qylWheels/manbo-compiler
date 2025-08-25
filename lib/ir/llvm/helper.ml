open Llvm

let is_ptr v = match v |> type_of |> classify_type with
  | Pointer -> true
  | _ -> false

let load_from_ptr ptr builder = build_load (operand ptr 0 |> type_of) ptr "" builder

(* 如果v是一个值，直接返回；如果v是一个指针，则读出指针所指向的值 *)
let value_or_load_value v builder = if is_ptr v then load_from_ptr v builder else v
