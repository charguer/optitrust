open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->

  !! Loop.grid_enumerate ~indices:["x"; "y";"z"] [cFor "idCell"];
)
