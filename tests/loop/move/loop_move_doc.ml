open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun () ->

  !! Loop.move [cFor "k"] ~before:[cFor "i"]

)
