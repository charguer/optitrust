open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
 (* TODO:
 internal:     Sequence_core.sub index nb p
 external:
  Sequence.sub  (path_to_first_item) nb  (using isolate_seq_and_last_dir)
  Sequence.sub_between (target_between_start) (target_between_stop)

examples:
  Sequence.sub  [ cInstr "int y" ] 2
  Sequence.sub_between [ cBefore; cInstr "int y" ] [ cAfter; cInstr "int z" ]
  Sequence.sub_between [cFunDef "main"; cStrict; cBody] [ cAfter; cInstr "int z" ]
 *)
  !! Sequence.sub 1 2 [cFunDef "main"; cStrict; cBody];
  !! Sequence.sub 3 1 [cFunDef "main"; cStrict; cBody];
  !! Sequence.sub 0 4 [cFunDef "main"; cStrict; cBody];
)
