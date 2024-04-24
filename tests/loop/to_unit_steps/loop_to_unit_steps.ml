open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.to_unit_steps ~index:"j" [cFor "i"];
  (* if not provided, the new index name is generated from the original one *)
  !! Trace.restore_original();
  !! Loop_basic.to_unit_steps [cFor "i"];
  )

(* LATER: we will one day need the transformation

  for (int i = 0; i < n; i++) {
    int x = 2*i;
  }

  to

  int x = 0;
  for (i..) {
    x += 2;


*)
