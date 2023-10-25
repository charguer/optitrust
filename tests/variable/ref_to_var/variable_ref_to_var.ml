open Optitrust
open Target


(* NOTE: We need first to call Loop_basic.hoist becuase reference can't be parsed my Menhir *)

let _ = Run.script_cpp (fun () ->

   !! Loop_basic.hoist [cVarDef "x"];
   !! Variable_basic.ref_to_var [cVarDef "x"];

   !! Loop_basic.hoist [cVarDef "z"];
   !! Variable_basic.ref_to_var [cVarDef "z"];

)
