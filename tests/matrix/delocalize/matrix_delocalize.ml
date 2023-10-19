open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
   !!  Matrix.delocalize "deposit" ~into:"depositThreads"
      ~acc:"sum" ~ops:(Ast.Local_arith (Lit_double 0., Binop_add))
      ~dim:(var "nbThreads") ~index:"idThread" ~indices:["idCell"]
      ~alloc_instr:[cVarDef "deposit"] [cFor "idCell"];
  )
"
#include <stdlib.h>

#include \"../../../include/optitrust.h\"

int nbParticlesInCell(int idCell);
int computeParticleDestination(int idCell, int i);

const int nbThreads = 4;
const double particleCharge = 1.0;
const int nbCells = 10;
const int nbSteps = 100;

int demo() {
  double* deposit = (double*) MALLOC1(nbCells, sizeof(int));
  for (int idStep = 0; idStep < nbSteps; idStep++) {
    for (int idCell = 0; idCell < nbCells; idCell++) {
      const int nb = nbParticlesInCell(idCell);
      for (int i = 0; i < nb; i++) {
        const int idCell2 = computeParticleDestination(idCell, i);
        deposit[MINDEX1(nbCells, idCell2)] += particleCharge;
      }
    }
  }
  return 0;
}
"

*)
