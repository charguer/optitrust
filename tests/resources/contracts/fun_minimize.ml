open Optitrust
open Target
open Resources

(*let _ = Flags.resource_errors_as_warnings := true*)
let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
    !! fun_minimize [cFunDef "unused_modifies"];
    !! fun_minimize [cFunDef "unused_reads"];
    !! fun_minimize [cFunDef "produced_uninit_used_ro"];
    !! fun_minimize [cFunDef "useless_pure_facts"];

    !! fun_minimize [cFunDef "merge_frac"];
    !! fun_minimize [cFunDef "multi_merge_frac"];
    !! fun_minimize [cFunDef "read_and_merge_frac"];
    !! fun_minimize [cFunDef "read_and_multi_merge_frac"];

    (* Typing of split_frac_generic is broken *)
    !! fun_minimize [cFunDef "split_frac_generic"];
    !! fun_minimize [cFunDef "split_subfrac_generic"];
    (* This contract does not respect the assumptions of minimize *)
    (* !! fun_minimize [cFunDef "split_frac_generic_lossy"]; *)
    !! fun_minimize [cFunDef "split_frac_specific"];
)
