open Optitrust
open Prelude

(* TODO: need to handle arbitrary code
  let _ = Flags.check_validity := true *)

let _ = Run.script_cpp (fun _ ->

  (* Demo with a single instruction *)
  !! If_basic.insert ~cond:(expr "x > 0") [cWriteVar "x"];

  (* Demo with a block *)
  !! Sequence_basic.intro ~mark:"foo" 2 [sInstr "b = 4"];
  !! If_basic.insert ~cond:(expr "x > 0") [cMark "foo"];

  (* Another demo with a block *)
  !! Trace.restore_original();
  !! Sequence_basic.intro ~mark:"new_block" 2 [sInstr "x = 5"];
  !! If_basic.insert ~cond:(expr "x > 0") [cMark "new_block"];

)
