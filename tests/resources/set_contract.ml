open Optitrust
open Target
open Syntax
open Resources_contract

let _ = Flags.dump_ast_details := true
(*let _ = Flags.bypass_cfeatures := true*)

let cell (x: var) =
  (new_anon_hyp (), formula_cell x)

let _ = Run.script_cpp (fun () ->
    let r = { pure = []; linear = [cell "a"]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = r ; post = r } [multi cFunDef ["incr"; "incr_twice"]];

    let r2 = { pure = []; linear = [cell "n"; cell "m"]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = r2 ; post = r2 } [cFunDef "incr_both"];

    show_res ();
)
