open Prelude

(** [insert ty tg]: expects the target [tg] to point at any expression that can be casted,
    then it will cast it to type [ty] *)
let insert (ty : typ) (tg : Target.target) =
  Target.reparse_after ~reparse:false (
  Target.apply_at_target_paths (Cast_core.insert_on ty)) tg
