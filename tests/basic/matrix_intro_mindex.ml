open Optitrust
open Target
open Syntax

let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix_basic.intro_mindex (trm_var "N") [nbMulti; cCellAccess ~base:[cVar "p"] ()];

)

"
int main () {

  const int N = 5;
  int p[5] = {0,1,2,3,4};
  p[0] = 10;
  int a = p[1];

  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.intro_mindex (trm_var "N") [nbMulti; cCellAccess ~base:[cVar "p"] ()];
  !! Matrix_basic.intro_mindex (trm_var "N") [cCellWrite ~base:[cVar "p"] ~index:[cTrue] (); dLHS]; (* [cCellWrite ~base:[cVar "p"] ~index:[cVar "i"] (); dLHS]; *)

)
