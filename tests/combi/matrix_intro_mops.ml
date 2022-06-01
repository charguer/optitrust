open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix.intro_mops (var "nbCells") [cVarDef "deposit"];
  
)

"
#include <stdlib.h>
int main() {
  const int nbCells = 10;
  double* deposit = (double*) malloc (nbCells * sizeof(int));
  for (int idCell = 0; idCell < nbCells; idCell++) {
    deposit[idCell] += 1.0;
  }
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Matrix.intro_mops (trm_var "N") [cVarDef "p"];
  !! Matrix.intro_mops (trm_var "N") [cVarDef "q"];

)
