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
   Tools.failure_expected (fun () ->
    !! Variable_basic.inline ~accept_functions:false [cFunDef "f"];);

   !! Variable_basic.inline [cFunDef "f"];

)
