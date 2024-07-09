open Optitrust
open Prelude

(* TODO: need to handle arbitrary code
  let _ = Flags.check_validity := true *)

let _ = Run.script_cpp (fun _ ->

  (* Demo with a single instruction *)
  !! If_basic.insert ~cond:(expr "x > 0") [cFunBody "f"; cWriteVar "x"];

  (* Demo with a block *)
  !! Sequence_basic.intro ~mark:"foo" 2 [cFunBody "f"; sInstr "b = 4"];
  !! If_basic.insert ~cond:(expr "x > 0") [cFunBody "f"; cMark "foo"];

  (* Another demo with a block *)
  !! Sequence_basic.intro ~mark:"new_block" 2 [cFunBody "g"; sInstr "x = 5"];
  !! If_basic.insert ~cond:(expr "x > 0") [cFunBody "g"; cMark "new_block"];

  !!! (); (* TODO: Find how to eliminate this reparse *)
)
