open Optitrust
open Prelude

(* let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true *)

(** Reproducing a subset of the PIC case study *)

let _ = Run.script_cpp (fun () ->

  let ctx = cFunBody "simulate_single_cell" in
  (* !! Resources.ensure_computed (); *)
  !! Resources.delete_annots [ctx];

  !! Function.inline [ctx; multi cFun ["matrix_vect_mul"]];
  !! Function.inline [ctx; multi cFun ["vect_add"; "vect_mul"]];

  !! Record.set_explicit [nbMulti; ctx; cFor "idStep"; cArrayWrite "localParticles"];
  !! Record.to_variables [ctx; multi cVarDef ["pos2"; "speed2"; "accel"]];
  (* !! List.iter (fun f -> Record.inline f [cTypDef "particle"]) ["speed"; "pos"]; *)

  !! Accesses.scale ~factor:(trm_float 2.0) [ctx; cVar "accelX"];
)
