open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Resources.specialize_arbitrary_fracs [cFunDef "one_fork"; cFor "j"; tBefore];
  !! Resources.specialize_arbitrary_fracs [cFunDef "two_forks"; cFor "j"; tBefore];
  !! Resources.specialize_arbitrary_fracs [cFunDef "two_forks"; cCall "ro_join_group"; occFirst; tAfter];
  !! Resources.specialize_arbitrary_fracs [cFunDef "two_forks_spe_twice"; cCall "ro_join_group"; occFirst; tAfter];
  !! Resources.specialize_arbitrary_fracs [cFunDef "two_forks_spe_twice"; cFor "j"; tBefore];
  !! Resources.specialize_arbitrary_fracs [cFunDef "fork_then_write"; cFor "j"; tBefore];
  !! Resources.specialize_arbitrary_fracs [cFunDef "read_then_fork"; cFor "j"; tBefore];
  !! Resources.specialize_arbitrary_fracs [cFunDef "write_then_fork"; cFor "j"; tBefore];
)
