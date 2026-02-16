open Ast
open Trm
open Typ
let magic_barrier_var = toplevel_var "magic_barrier"
let magic_barrier (): trm =
  trm_apps (trm_var magic_barrier_var) []

let magic_barrier_inv (t: trm): unit option =
  match (trm_apps_inv t) with
  | Some ({desc = Trm_var v}, _) when (var_eq v magic_barrier_var) -> Some ()
  | _ -> None

let all_mem_ok_var = toplevel_var "all_mem_ok"
