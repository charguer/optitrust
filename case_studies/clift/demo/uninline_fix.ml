open Optitrust
open Prelude

let _ =
  Flags.check_validity := true;
  Flags.detailed_resources_in_trace := true;
  Flags.save_ast_for_steps := Some Steps_all

let _ = Flags.recompute_resources_between_steps := true

let _ =
  Run.script_cpp (fun _ ->
    (* !!  Matrix.reorder_dims ~order:[1;0] [cVarDef "x"]; *)

    !! Function.uninline ~f:[cFunDef "f"] [cFunDef "g"; cCellWrite ~base:[cVar "x"] ()  ];
    !! Function.uninline ~f:[cFunDef "f2"] [cFunDef "g2"; cFor "j"];
    )
