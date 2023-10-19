open Optitrust
open Target
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop.slides ~size_steps:[
    Some (trm_int 2, trm_int 1);
    Some (trm_int 3, trm_int 2);
  ] [cFor "i"];
)
