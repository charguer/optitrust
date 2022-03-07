open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->
   
  !! Variable_basic.bind "c" ~const:true ~is_ptr:true [cFun "test_pointer_arg"; dArg 0];
  !! Variable_basic.bind "a" ~const:true [cFunDef "test"; cReturn; cArrayInit];
  !! Variable_basic.bind "b" [cVarDef "x"; cArrayInit];
)
 