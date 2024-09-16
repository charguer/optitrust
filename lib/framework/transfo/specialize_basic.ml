open Prelude
open Target

(** [any e tg]: expects the target [tg] to be point at a call to the function [ANY], then it will replace it with [e]. *)
let any (e : trm) : target -> unit =
  Target.apply_at_target_paths (Specialize_core.any_on e)

(** [fundefs spec_name spec_args tg] *)
let fundefs (spec_name : string) (spec_args : (trm option) list) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    apply_at_target_paths (Specialize_core.fun_def_on spec_name spec_args) tg)

(** [funcalls spec_name args_to_choose tg]: expects the target [ŧg] to point to a function call, and assumes that
      there is already a function generated by the transformation [fundefs], then it will replace that
      call with a call to the function [spec_name] already defined either by using the transformation [fundefs],
      Or manually by the user.*)
let funcalls (spec_name : var) (args_to_choose : bool list) : target -> unit =
  apply_at_target_paths (Specialize_core.fun_call_on spec_name args_to_choose)