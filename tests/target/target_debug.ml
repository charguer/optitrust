open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* show [cFieldRead "x" ]; *)
  show [cFieldWrite "x" ];
  show [cFieldRead "y" ];
  show [cFieldWrite "y" ];
  show [cFieldRead "pos" ];
  show [cFieldWrite "pos" ];
  

)
