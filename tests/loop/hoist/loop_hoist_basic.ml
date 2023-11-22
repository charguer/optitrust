open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->
  !! ();
  Resources.show ();
  let x = find_var_in_current_ast "x" in
  let z = find_var_in_current_ast "z" in
  let y = find_var_in_current_ast "y" in
  !! Matrix_basic.intro_malloc0 x [cFunBody "f"; cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 z [cFunBody "f"; cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 y [cFunBody "f"; cFor "n"; dBody];
  !! Loop_basic.hoist [cFunBody "f"; cVarDef "x"];
  !! Loop_basic.hoist [cFunBody "f"; cVarDef "z"];
  (*
  !! Ast.assert_transfo_error "expected uninitialized allocation" (fun _ ->
    Loop_basic.hoist [cVarDef "w"]);
  *)

  !! Loop_basic.hoist ~name:"yn" [cFunBody "f"; cVarDef "y"];
  !! Loop_basic.hoist ~name:"ym" [cFunBody "f"; cVarDef "yn"];
  !! Loop_basic.hoist ~name:"yl" [cFunBody "f"; cVarDef "ym"];

  let y = find_var_in_current_ast "sum" in
  !! Matrix_basic.intro_malloc0 y [cFunBody "f2"; cFor "j"; dBody];
  !! Loop_basic.hoist [cFunBody "f2"; cVarDef "sum"];

  (* TODO: Loop_basic.hoist cannot recheck resources because we don't handle aliases *)
  !!! ();
)
