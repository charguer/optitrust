open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* Demo with a single instruction *)
  !! Generic_basic.arbitrary_if "x > 0" [sInstr "x = 5"];

  (* Demo with a block *)
  !! Sequence_basic.intro ~label:"foo" 2 [sInstr "b = 4"];
  !! Generic_basic.arbitrary_if "x > 0" [cLabel "foo"];
     Label_basic.remove [cLabel "foo"];
  (* LATER: investigate why the error shows up saying that foo appears twice,
     since we don't ask for a reparse *)

  (* Another demo with a block *)
  !! Trace.alternative (fun () ->
      !! Sequence_basic.intro 2 [sInstr "x = 5"];
      !! Generic_basic.arbitrary_if "x > 0" [cSeq ~args_pred:(target_list_one_st (sInstr "x = 5")) ()];
      !!();
  )
)
