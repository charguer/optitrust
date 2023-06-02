open Optitrust
open Target
open Ast

let _ = Flags.dump_ast_details := true
let _ = Flags.use_clang_format := false
(*let _ = Flags.bypass_cfeatures := true*)

let cell ?(name: string option) (t: trm) =
  (name, trm_apps (var "Cell") [t])

let _ = Run.script_cpp (fun () ->
    let r = { pure = []; linear = [cell (var "a")]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = r ; post = r } [multi cFunDef ["incr"; "incr_twice"]];

    let r2 = { pure = []; linear = [cell (var "n"); cell (var "m")]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = r2 ; post = r2 } [cFunDef "incr_both"];

    show_res ();
)
