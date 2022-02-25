
open Optitrust
open Target

let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp ~parser:Parsers.Menhir (fun _ ->

    !! Function_basic.uninline ~fct:[cFunDef "iter_nat_for"] [cLabelBody "hobody"];

    !! Function_basic.uninline ~fct:[cFunDef "iter_bag2"] [cLabelBody "bagbody2"];
    (* Test to undo the action of the unlining: *)
      !! Function_basic.inline [cFun "iter_bag2"];
      !! Function_basic.beta [cTopFunDef "test_bag2"; cFor_c ""; dBody; cFun""];

    !! Function_basic.uninline ~fct:[cFunDef "iter_bag"] [cLabelBody "bagbody"];
      !! Function_basic.inline [cFun "iter_bag"];
      !! Function_basic.beta [cTopFunDef "test_bag"; cFor_c ""; dBody; cFun""];
)
