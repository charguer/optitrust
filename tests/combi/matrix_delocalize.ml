open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
   !!  Matrix.delocalize "deposit" ~into:"depositCorners"
      ~acc:"sum" ~ops:(Ast.Local_arith (Lit_double 0., Binop_add))
      ~dim:(var "nbCorners") ~index:"idCorner" ~indices:["idCell"]
      ~alloc_instr:[cVarDef "deposit"] [cFor "idCell"];
  )
"
#include <stdlib.h>

#include \"../../include/optitrust.h\"

int nbParticlesInCell(int idCell);
int computeParticleDestination(int idCell, int i);

typedef struct { int v[8]; } res;
res indicesOfCorners(int idCell);

const int nbCorners = 8;
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
        for (int k = 0; k < 8; k++) {
          deposit[MINDEX1(nbCells, indicesOfCorners(idCell2).v[k])] += 1.0;
        }
      }
    }
  }
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->


   !! Matrix.delocalize "a" ~into:"x" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cFor "i"];

   let alloc_instr = [cTopFunDef "allocate"; cWriteVar "b"] in
   !! Matrix.delocalize "b" ~into:"y" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) ~alloc_instr ~labels:["alloc";"";"dealloc"] [cFor "j"];

   !! Trace.alternative (fun () ->
        !! Matrix.delocalize "a" ~into:"x" ~last:true  ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cFor "i"];
        !!());

  !! Trace.alternative (fun () ->
        !! Matrix.delocalize "a" ~use:(Some (expr"k")) ~into:"x" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cFor "i"];
        !!());

)


(*
let _ = Run.doc_script_cpp (fun _ ->
   !!  Matrix.delocalize "deposit" ~into:"depositThreads"
      ~acc:"sum" ~ops:(Ast.Local_arith (Lit_double 0., Binop_add))
      ~dim:(var "nbThreads") ~index:"idThread" ~indices:["idCell"]
      ~alloc_instr:[cVarDef "deposit"] [cFor "idCell"];
  )
"
#include <stdlib.h>

#include \"../../include/optitrust.h\"

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