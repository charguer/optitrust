open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  
  !! Variable_basic.inline [cVarDef "a"];

)

"
int main() {
  const int a = 3;
  const int b = 4;
  int r = a + a + b;
}
"
let _ = Run.script_cpp (fun _ ->

  (* for variables *)
  !! Variable_basic.inline [cVarDef "a"];

  (* for functions *)
  !! Variable_basic.inline ~accept_functions:true [cFunDef "f"];

  (* if accept_functions is false then transformation will fail *)
  Tools.failure_expected (fun () ->
   !! Variable_basic.inline [cFunDef "f"];);

  (* tranformation fails for non const variables *)
  Tools.failure_expected (fun () ->
   !! Variable_basic.inline [cVarDef "c"];)

)
