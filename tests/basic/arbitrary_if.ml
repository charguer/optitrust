open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* Demo with a single instruction *)
  !! Flow.insert_if "x > 0" [sInstr "x = 5"];

  (* Demo with a block *)
  !! Sequence_basic.intro ~label:"foo" 2 [sInstr "b = 4"];
  !! Flow.insert_if "x > 0" [cLabel "foo"; dBody];
  !! Label_basic.remove [cLabel "foo"];

  (* Another demo with a block *)
  !! Trace.alternative (fun () ->
      !! Sequence_basic.intro 2 [sInstr "x = 5"];
      !! Flow.insert_if "x > 0" [cSeq ~args_pred:(target_list_one_st [sInstr "x = 5"]) ()];
      !!();
  )

)

(* LATER: investigate why there is a call to reparse just before the removal of the label
     when executing
     !! Flow.insert_if "x > 0" [cLabel "foo"];
        Label_basic.remove [cLabel "foo"]; *)
