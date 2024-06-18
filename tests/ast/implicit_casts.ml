open Optitrust
open Target

let _ = Flags.print_cast_origin := true

let _ = Run.script_cpp ~filename:"c_lang.cpp" (fun () ->
  !! ()
)

