open Optitrust
open Target

(* TODO: the operations don't seem to work here; an issue with the target? *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Arith_basic.(simpl gather) [cVarDef "a"; dBody];
       Arith_basic.(simpl gather) [cVarDef "b"; dBody];
       Arith_basic.(simpl gather) [cVarDef "c"; dBody];
       Arith_basic.(simpl expand) [cVarDef "d"; dBody];
  )
"
int main() {
  int a = 2 + 3;
  int b = 3 * a + 4 * a;
  int c = (a * 3 * a) / a;
  int d = (a + b) * c;
}
"



(* LATER: add a "compute" transformation to simplify
    - products of int
    - sums and products of doubles

   LATER: simplification recursively in atoms, see example of [w];
   to implement using trm_map. *)


let _ = Run.script_cpp (fun _ ->

    !! Arith_basic.(simpl identity) [nbMulti; cWriteVar "x"; dRHS];
    !! Arith_basic.(simpl normalize) [nbMulti; cWriteVar "x"; dRHS];
    !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "y"; dRHS];
    !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "z"; dRHS];
    !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "t"; dRHS];
    !! Arith_basic.(simpl expand) [nbMulti; cWriteVar "u"; dRHS];
    !! Arith_basic.(simpl expand) [nbMulti; cWriteVar "v"; dRHS];

(* For testing one line, add a line in the source "r = ...;" and use:
    !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "r"; dRHS];
*)

(* For testing all lines concerning one variable:
    !! (for i = 0 to 5 do
         Printf.printf "===%d====\n" i;
         Arith_basic.(simpl gather_rec) [occIndex i; cWriteVar "t"; dRHS];
    done);
*)

(* For debugging apply_bottom_up:
     Arith_basic.(simpl (apply_bottom_up_if_debug true true)) [occIndex i; cWriteVar "t"; dRHS];
     Arith_basic.(simpl apply_bottom_up_debug) [occIndex i; cWriteVar "t"; dRHS];
*)


)