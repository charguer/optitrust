open Optitrust
open Syntax
open Resource_contract

let _ = Run.script_cpp (fun () ->
    !! Resources.set_fun_contract (parse_fun_contract [__modifies("a ~> Cell")]) [multi cFunDef ["incr"; "incr_twice"]];

    !! Resources.set_fun_contract (parse_fun_contract [__modifies("n ~> Cell"); __modifies("m ~> Cell")]) [cFunDef "incr_both"];

    (* Should produce empty diff: *)
    !! Resources.set_fun_contract (parse_fun_contract [__modifies("n ~> Cell, m ~> Cell")]) [cFunDef "incr_both"];

    !! Resources.set_fun_contract (parse_fun_contract [__modifies("m ~> Matrix1(sz)")]) [cFunDef "incr_range"];
    !! Resources.set_loop_contract (parse_loop_contract [__modifies("&m[MINDEX1(sz, i)] ~> Cell")]) [cFor "i"];

    (*!! Trace.apply Scope.infer_var_ids;
    !! Trace.apply Scope.unique_alpha_rename;*)

    Resources.show ();
)
