open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.unroll ~inner_braces:true [cFor "i"];
  !! Loop_basic.unroll ~inner_braces:true [cFor "j"];

  (* following is not OK because of C re-definition. *)
  !! Trace.failure_expected (fun () ->
    Loop_basic.unroll ~inner_braces:false [cFor "k"]);

  (* TODO: test unroll on SIMD loop *)
)
