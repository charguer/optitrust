open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  show [cFieldGet "x" ];
  show [cFieldSet "x" ];
  show [cFieldGet "y" ];
  show [cFieldSet "y" ];
  show [cFieldGet "pos" ];
  show [cFieldSet "pos" ];
  

)
