(* Usage: 'make perf_out.cpp' then read 'timing.log'
  Trick: add to optitrust_flags.sh the line
   NODIFFDISPLAY=1
*)

open Optitrust
open Target

let _ = Flags.analyse_time_details := true

let _ = Run.script_cpp (fun () ->

    (* !! Variable.inline [nbMulti; cTop "main"; cVarDef "idCell3" ];*)
    !! Variable.inline [nbMulti; cTop "main"; sInstr "idCell3 =" ]
    (*!! Variable.inline [cVarDef "accel" ]*)
    (*!! Variable.inline [cTop "main"; cVarDef "accel" ]*)
    )

