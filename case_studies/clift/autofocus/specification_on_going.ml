open Optitrust
open Ast
open Trm
open Prelude

let _ = Flags.check_validity := true; Flags.recompute_resources_between_steps := false
let _ = Run.script_cpp (fun _ -> !!!(););
