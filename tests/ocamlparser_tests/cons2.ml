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

let intro_derived_constructor (trm_from : trm) (trm_to : trm) tg : unit =
  (*explore the trm_from and trm_to to get the expected variables? *)



  let x : var = pvar "x" in
  let y : var = pvar "y" in
  let t : var = pvar "t" in

  let typed_x : typed_var = into_typed_var x in
  let typed_y : typed_var = into_typed_var y in
  let typed_t : typed_var = into_typed_var t in

  (*
  let cons = trm_var (name_to_var "Cons") in
  let cons2 = trm_var (name_to_var "Cons2") in
  *)

  let rule_vars : typed_var list = [typed_x; typed_y; typed_t(*; cons; cons2*)] in


  let mark = "" in

  let rule =
    {rule_vars; rule_aux_vars = []; rule_from = trm_from; rule_to = trm_to}
  in
  Target.apply_at_target_paths (Rewrite_core.apply_rule_bottom_up ~mark rule) tg



let intro_cons2 tg : unit =

  (* let var_cons = toplevel_var "Cons" *)

   (* use new_var for pattern vars TODO
      and put it in the pattern namespace
   *)
  let pattern_x : trm = trm_pattern_var "x" in
  let pattern_y : trm = trm_pattern_var "y" in
  let pattern_t : trm = trm_pattern_var "t" in

  let cons = trm_var (name_to_var "Cons") in
  let cons2 = trm_var (name_to_var "Cons2") in

  let trm_from = trm_apps cons [pattern_x; trm_apps cons [pattern_y; pattern_t]] in
  let trm_to = trm_apps cons2 [pattern_x; pattern_y; pattern_t] in
  (* print_rule with flags id, then rewrite *)

  (* intro_derived_constructor trm_from trm_to tg *)
 ignore (trm_from,trm_to,tg,intro_derived_constructor)

let _ = Run.script_ml (fun () ->
  !! intro_cons2 [dRoot]
)
