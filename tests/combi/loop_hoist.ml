open Optitrust
open Target


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
  !! Loop.hoist ~inline:false [cFor "i"; cVarDef "x"];
  !! Loop.hoist [cFor "j"; cVarDef "y"];
  !! Loop.hoist ~inline:false ~name:"xk" [cFor "k"; cVarDef "x"];
  !! Loop.hoist ~nest_of:2 [cFor "l"; cVarDef "x"];
  !! Loop.hoist ~nest_of:3 ~name:"xa" [cFor "a"; cVarDef "x"];
  !! Loop.hoist_alloc ~indep:["mi"] ~dest:[tBefore; cFor "mi"] [cVarDef "m"];
  !!! ();
)
