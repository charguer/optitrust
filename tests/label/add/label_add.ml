open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

   !! Label_basic.add "start" [cWriteVar "x"] ;
   !! Trace.alternative (fun () ->
      !! Label_basic.new_add "start" [cWriteVar "x"] ;
      !!(););

   !! Label_basic.add "cond" [cIf ()];

   !! Trace.alternative (fun () ->
      !! Label_basic.new_add "incr" [nbMulti; sInstr "x++"];
      !!(););

   !! Label_basic.add "incr_1" [cIf (); sInstr "x++"];
   !! Label_basic.add "incr_2" [cIf (); sInstr "x--" ];
   !! Label_basic.add "stop" [cReturn];

)
