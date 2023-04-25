open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Matrix_basic.intro_malloc0 "x" [cFor "i"; dBody];
  !! Loop_basic.hoist [cVarDef "x"];
)

"
int main() {
  for (int i = 0; (i < 4); i++) {
    int x;
    x = (2 * i);
  }
}
"


let _ = Run.script_cpp (fun () ->
  !! Matrix_basic.intro_malloc0 "x" [cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 "z" [cFor "i"; dBody];
  !! Matrix_basic.intro_malloc0 "y" [cFor "n"; dBody];

  !! Loop_basic.hoist [cVarDef "x"];
  !! Loop_basic.hoist [cVarDef "z"];
  (*
  !! Ast.assert_transfo_error "expected uninitialized allocation" (fun _ -> 
    Loop_basic.hoist [cVarDef "w"]);
*)

  !! Loop_basic.hoist ~name:"yn" [cVarDef "y"];
  !! Loop_basic.hoist ~name:"ym" [cVarDef "yn"];
  !! Loop_basic.hoist ~name:"yl" [cVarDef "ym"];

  !!! ();
)
