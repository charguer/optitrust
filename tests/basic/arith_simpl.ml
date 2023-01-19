open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  
  !! Arith_basic.(simpl gather) [cVarInit "a"];
  !! Arith_basic.(simpl gather) [cVarInit "b"];
  !! Arith_basic.(simpl gather) [cVarInit "c"];
  !! Arith_basic.(simpl expand) [cVarInit "d"];

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
  
  (* !! Arith_basic.simplify ~indepth:true [dRoot]); *) (* Test of all at once: *)

  !! Arith_basic.nosimpl [nbMulti; cFunDef "simpl_in_depth"; cVarDef "x"; cFun "g"];
  !! Arith_basic.simplify ~indepth:true [nbMulti; cFunDef "simpl_in_depth"; cVarDef "x"];
  !! Arith_basic.clear_nosimpl [];

  !! Arith_basic.simplify ~indepth:false [nbMulti; cFunDef "simpl_in_depth"]; (* do nothing *)
  !! Arith_basic.simplify ~indepth:true [nbMulti; cFunDef "simpl_in_depth"];

  !! Arith_basic.(simpl identity) [nbMulti; cWriteVar "x"; dRHS];
  !! Arith_basic.(simpl normalize) [nbMulti; cWriteVar "x"; dRHS];
  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "y"; dRHS];
  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "z"; dRHS];
  !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "t"; dRHS];
  !! Arith_basic.(simpl expand) [nbMulti; cWriteVar "u"; dRHS];
  !! Arith_basic.(simpl expand) [nbMulti; cWriteVar "v"; dRHS];

  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "ls"; dRHS];
  !! Arith_basic.(simpl gather) [nbMulti; cFor "ls2"; dForStop];
  !! Arith_basic.(simpl gather) [nbMulti; cFor "ls2"; dForStart];
)


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
