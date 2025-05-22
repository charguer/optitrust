open Optitrust
open Prelude

open Trm_matching

let _ = Ast.behavior_ocaml := true
let _ = Flags.debug_var_id := true

let _ = Flags.c_parser_name := "Ocaml_parser"

let into_typed_var ?typ (v : var) =
  match typ with
  | None -> (v, typ_auto)
  | Some t -> (v, t)

let intro_cons2 tg : unit =

  let x : var = pvar "x" in
  let y : var = pvar "y" in
  let t : var = pvar "t" in

  let typed_x : typed_var = into_typed_var x in
  let typed_y : typed_var = into_typed_var y in
  let typed_t : typed_var = into_typed_var t in


  let cons = trm_toplevel_var "Cons" in
  let cons2 = trm_toplevel_var "Cons2" in

  let cons_id =
    match trm_var_inv cons with
    | Some v -> v.id
    | _ -> assert false
  in

  let cons2_id =
    match trm_var_inv cons2 with
    | Some v -> v.id
    | _ -> assert false
  in


  Printf.printf "Ccons id : %d\n Cons2 id : %d\n" cons_id cons2_id;

  let rule_vars : typed_var list = [typed_x; typed_y; typed_t] in

  let mark = "" in

  let trm_x = trm_var x in
  let trm_y = trm_var y in
  let trm_t = trm_var t in

  let trm_from = trm_apps cons [trm_x; trm_apps cons [trm_y; trm_t]] in
  let trm_to = trm_apps cons2 [trm_x; trm_y; trm_t] in

  let rule =
    {rule_vars; rule_aux_vars = []; rule_from = trm_from; rule_to = trm_to}
  in
  Target.apply_at_target_paths (Rewrite_core.apply_rule_bottom_up ~mark rule) tg

let _ = Run.script_ml (fun () ->
  !! intro_cons2 [dRoot]
)
