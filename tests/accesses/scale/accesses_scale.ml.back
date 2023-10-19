open Optitrust
open Target
open Prelude

(* Arthur: we should be more consistent with ~addr: and ~base:  => perhpas addr is better?
   Begatim: we use base for array and struct accesses and we use addr for variables *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Accesses.scale ~factor:(trm_double 5.0) [cVarDef "y"; cVar "x"];
  )
"
int main() {
  int x = 2;
  int y = x;
  x = y;
}
"


let _ = Run.script_cpp (fun _ ->

  !! Accesses.scale ~factor:(trm_double 5.0) [nbMulti; cTopFunDef "test_var"; cVar "x"];
  !! Accesses.scale ~factor:(trm_double 5.0) [nbMulti; cTopFunDef "test_array"; sExpr "t[0]"];
  !! Accesses.scale ~factor:(trm_double 5.0) [nbMulti; cTopFunDef "main"; sExpr "t[i]"];

)
