open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.save_ast_for_steps := Some Steps_all

let _ = Run.script_cpp (fun () ->
  !! Show.At.trm ~style:(Style.internal_ast_only_desc()) [cFunDef "f"] ;
  !! Ghost_pure.minimize_all_in_seq [cFunBody "f"];
  !! Ghost_pure.move_surrounding_inside [cFor "i"];
)
