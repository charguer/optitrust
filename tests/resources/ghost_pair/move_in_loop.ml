open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ =
  Run.script_cpp (fun _ ->
      !!Ghost_pair.move_in_loop [ cFunDef "simple_ghost_in"; cFor "i" ];
      !!Ghost_pair.move_in_loop [ cFunDef "delete_sreads"; cFor "i" ];
      (* !!move_ghost_pair_in [ cFunDef "ghost_in_invariant"; cFor "i" ]; *)
      !!())
