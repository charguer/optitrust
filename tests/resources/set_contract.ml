open Optitrust
open Target
open Ast

let _ = Flags.dump_ast_details := true
let _ = Flags.use_clang_format := false
(*let _ = Flags.bypass_cfeatures := true*)

let cell ?(name: string option) (t: trm) =
  (name, trm_apps (var "Cell") [t])


let _ = Run.script_cpp (fun () ->
(*    !! Resources.set_fun_contract { pre = Some { pure = [(Some "a", trm_apps (var "array") [trm_var "p"] )]; linear = [] }; post = None } [cFunDef "incr"]
 *    *)
    !! show [cFunDef "main"];

    let r = { pure = []; linear = [cell (var "a")]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = Some r ; post = Some r } [multi cFunDef ["incr"; "incr_twice"]];

    let r2 = { pure = []; linear = [cell (var "n"); cell (var "m")]; fun_contracts = Var_map.empty } in
    !! Resources.set_fun_contract { pre = Some r2 ; post = Some r2 } [cFunDef "incr_both"];
    Printf.eprintf "%s\n\n" (AstC_to_c.ast_to_string (Ast_fromto_AstC.contract_intro (Trace.ast ())));

    show_res ();

    !! ();
)
