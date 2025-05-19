open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  bigstep "x";
  (*!! Resources.ensure_computed ();*)

  (* First, tile on i and j.*)
  !! Loop_basic.tile (trm_int 4) ~index:"bi" ~bound:TileDivides [cFunDef "matrix2_copy"; cFor "i"];
  !! Loop_basic.tile (trm_int 4) ~index:"bj" ~bound:TileDivides [cFunDef "matrix2_copy"; cFor "j"];
  (*!! Resources.ensure_computed ();*)

  (* Then, color the blocks. *)
  !! Loop_basic.color (trm_int 2) ~index:"ci" [cFor "bi"];
  !! Loop_basic.color (trm_int 2) ~index:"cj" [cFor "bj"];
  (*!! Resources.ensure_computed ();*)


  (* At this point, the loops are build but in the wrong order. *)
  (* We want the order to be ci - cj - bi - bj -  i -  j. *)
  (* Currently:              ci - bi -  i - cj - bj -  j. *)

  (* The two following swaps take the cj loop out of the loops on i and bi.
  (!! Loop.swap [cFor "i"];)
  (* After:                  ci - bi - cj -  i - bj -  j. *)
  !! Loop.swap [cFunBody "matrix2_copy"; cFor "bi"];
  (* After:                  ci - cj - bi -  i - bj -  j. *)

  (* This last take the bj loop out of the loop on i.*)
  !! Loop.swap [cFunBody "matrix2_copy"; cFor "i"];

  !! Resources.ensure_computed ();*)

  !! Loop.reorder ~order:["cj";"bi"] [cFor "bi"];
)
