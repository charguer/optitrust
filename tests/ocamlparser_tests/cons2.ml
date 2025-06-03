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

let pattern_namespace = "Pattern__"
let pnew_var name = new_var ~namespaces:[pattern_namespace] name

let intro_cons2 (tg : target) : unit =

  let x : var = pnew_var "x" in
  let y : var = pnew_var "y" in
  let t : var = pnew_var "t" in

  let typed_x : typed_var = into_typed_var x in
  let typed_y : typed_var = into_typed_var y in
  let typed_t : typed_var = into_typed_var t in


  let cons = trm_toplevel_var "Cons" in
  let cons2 = trm_toplevel_var "Cons2" in
(*
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


  Printf.printf "\n\nCons id : %d\nCons2 id : %d\n" cons_id cons2_id;
 *)
  let rule_vars : typed_var list = [typed_x; typed_y; typed_t] in

  let mark = "" in

  let trm_x = trm_var x in
  let trm_y = trm_var y in
  let trm_t = trm_var t in

  let trm_from = trm_apps cons [trm_x; trm_apps cons [trm_y; trm_t]] in
  let trm_to = trm_apps cons2 [trm_x; trm_y; trm_t] in

   Printf.printf "\ntrm_from : \n";
    (*changed this from (Ast_to_c.default_style ()) to Ast_to_text.default_style*)
    let s = Ast_to_c.default_style () in
    let ast_style = s in
    print_string (Ast_to_c.ast_to_string ~style:ast_style trm_from);


  Printf.printf "\ntrm_to : \n";
    (*changed this from (Ast_to_c.default_style ()) to Ast_to_text.default_style*)
    let s = Ast_to_c.default_style () in
    let ast_style = s in
    print_string (Ast_to_c.ast_to_string ~style:ast_style trm_to);

  Printf.printf "\n";

  let rule =
    {rule_vars; rule_aux_vars = []; rule_from = trm_from; rule_to = trm_to}
  in
  Target.apply_at_target_paths (Rewrite_core.apply_rule_bottom_up ~mark rule) tg

let _ = Run.script_ml (fun () ->
  !! intro_cons2 [] (* [nbMulti; dRoot] *) (*I have honestly no idea why this works... is there a link with apply_rule_bottom_up overwriting the actual path? *)
)
