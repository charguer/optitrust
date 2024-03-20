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
    !! Resources.(set_loop_contract [__sequentially_modifies("acc ~> Cell")]) [cFunBody "loop"; cFor "j"];

    !! Resources.(set_loop_contract ~strict:false [__reads("&M[MINDEX1(n, j)] ~> Cell")]) [cFunBody "non_strict"; cFor "j"];
    (*!! Trace.apply Scope.unique_alpha_rename;*)

    Resources.show ();
)
