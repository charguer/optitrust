open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.report_exectime := true

let _ = Run.script_cpp (fun () ->

  !! (); (** Typedef_basic.insert "T" (Typedef_alias typ_int) [tBefore; cFunDef "mm"]; *)

)

