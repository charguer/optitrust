open Optitrust
open Prelude
let show = Show.add_marks_for_target_unit_tests


let _ = Run.script_cpp (fun () ->

  !! show [ tSpan [tBefore; cVarDef "a"] [tBefore; cVarDef "b"] ];

  !! show [ tSpan [tBefore; cPlusEq ~rhs:[cInt 1] ()] [tAfter; cPlusEq ~rhs:[cInt 2] ()] ];

  !! show [ cFor "i"; tSpan [tBefore; cPlusEq ~rhs:[cInt 1] ()] [tAfter; cPlusEq ~rhs:[cInt 2] ()] ];

  !! show [ tSpan [tFirst] [tLast] ];

  !! show [ tSpan [tAfter; occFirst; cPlusEq ()] [tBefore; occLast; cPlusEq ()] ];

)
