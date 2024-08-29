open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->
  let ctx = cFunBody "simulate_single_cell" in
  let stepDuration = trm_var (find_var_in_current_ast ~target:[ctx] "stepDuration") in

  !! Accesses.scale ~factor:stepDuration [nbMulti; ctx; cReadOrWrite ~addr:[cAccesses ~base:[cVar "lParticles"] ~accesses:[cField ~field:"speed" (); cField ()] ()] ()];

  bigstep "finish with style";
  !! Variable.inline [ctx; cVarDefs ["accelX"; "accelY"; "accelZ"; "pos2X"; "pos2Y"; "pos2Z"]];
  !!! Arith.(simpls_rec [expand; gather_rec]) [ctx];
  !! Function.use_infix_ops ~indepth:true [ctx];
  !! Cleanup.std ();

  (* TODO:
    - cleanup script
    - bind pointer to lParticles cell?
    - inline cornerInterpolationCoeff to allow array on stack
  *)
)
