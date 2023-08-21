open Optitrust
open Syntax


let _ = Run.doc_script_cpp (fun () ->

  (* !! Loop.hoist [cFor "i"; cVarDef "x"]; *)
  ()

)

"
int* t;
int* u;

int main (){
  for (int i = 0; i < 10; i++) {
    int x = t[i];
    u[i] = x;
    int z = x;
  }
}
"

let _ = Run.script_cpp (fun () ->
  !! Loop.hoist_expr ~dest:[tBefore; cFor "i"] "t2" [cFor "i"; cArrayRead "t"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "l"] "t02" ~indep:["l"; "m"] [cFor "l"; cArrayRead "t"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "a"] "a2" ~indep:["b"; "c"] [cVarDef "x"; cVar "a"];
  !!! ();
)
