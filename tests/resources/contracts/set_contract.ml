open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->
    !! Resources.(set_fun_contract [__modifies("a ~> Cell")]) [multi cFunDef ["incr"; "incr_twice"]];

    !! Resources.(set_fun_contract [__modifies("n ~> Cell"); __modifies("m ~> Cell")]) [cFunDef "incr_both"];

    (* Should produce empty diff: *)
    !! Resources.(set_fun_contract [__modifies("n ~> Cell, m ~> Cell")]) [cFunDef "incr_both"];

    !! Resources.(set_fun_contract [__modifies("m ~> Matrix1(sz)")]) [cFunDef "incr_range"];
    !! Resources.(set_loop_contract [__modifies("&m[MINDEX1(sz, i)] ~> Cell")]) [cFor "i"];

    (* Weird behavior because acc is already stack-var encoded when parsing the contract:
       we need to write `acc` and not `&acc` *)
    !! Resources.(set_loop_contract [__sequentially_modifies("acc ~> Cell")]) [cFor "j"];

    (*!! Trace.apply Scope.infer_var_ids;
    !! Trace.apply Scope.unique_alpha_rename;*)

    Resources.show ();
)
