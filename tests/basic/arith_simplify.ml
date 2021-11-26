open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    (*!! Arith_basic.(simpl identity) [nbMulti; cWriteVar "x"; dRHS];*)
    !! Arith_basic.normalize [nbMulti; cWriteVar ""; dRHS];
    (*
*)
    (*
    !! (for i = 0 to 5 do
         Printf.printf "===%d====\n" i;
         Arith_basic.(simpl gather_rec) [occIndex i; cWriteVar "t"; dRHS];
    done);*)

        (* Arith_basic.(simpl (apply_bottom_up_if_debug true true)) [occIndex i; cWriteVar "t"; dRHS];*)
        (* Arith_basic.(simpl apply_bottom_up_debug) [occIndex i; cWriteVar "t"; dRHS]; *)
        (* Arith_basic.normalize [occIndex i; cWriteVar "t"; dRHS];*)

  (*
    !! Arith_basic.normalize [occIndex 3; cWriteVar "t"; dRHS];
  *)
    (*
    !! Arith_basic.normalize [nbMulti; cWriteVar "x"; dRHS];
    !! Arith_basic.(simpl gather_one) [nbMulti; cWriteVar "y"; dRHS];
    !! Arith_basic.(simpl (fun t -> normalize_one (gather_one t))) [nbMulti; cWriteVar "z"; dRHS];
    *)

)