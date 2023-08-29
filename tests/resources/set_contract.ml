open Optitrust
open Syntax
open Resources_contract

let _ = Flags.dump_ast_details := true
(*let _ = Flags.bypass_cfeatures := true*)

let cell (x: string) =
  (new_anon_hyp (), formula_cell (name_to_var x))

let _ = Run.script_cpp (fun () ->
    let r = { pure = []; linear = [cell "a"]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = r ; post = r } [multi cFunDef ["incr"; "incr_twice"]];

    let r2 = { pure = []; linear = [cell "n"; cell "m"]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = r2 ; post = r2 } [cFunDef "incr_both"];

    Resources.show ();
)
