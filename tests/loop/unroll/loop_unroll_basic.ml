open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.unroll [cFunBody "simple"; cFor "i"];

  !! Loop_basic.unroll [cFunBody "name_conflict_no_brace"; cFor "i"];
  !! Loop_basic.unroll ~inner_braces:true [cFunBody "name_conflict_with_braces"; cFor "i"];

  !! Loop_basic.unroll [cFunBody "start_add"; cFor "i"];

  !! Loop_basic.unroll [nbMulti; cFunBody "step"; cFor "i"];

  (* TODO: test unroll on SIMD loop *)
)
