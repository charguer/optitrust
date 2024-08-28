open Prelude
open Target

(** [any e tg]: expects the target [tg] to be point at a call to the function [ANY], then it will replace it with [e]. *)
let any (e : trm) : target -> unit =
  Target.apply_at_target_paths (Specialize_core.any_on e)

(** [choose_fct select_arg]: expects the target [tg] to point at a call to the function [CHOOSE]
    which is used in the delocalize transformation (see to [Variable.delocalize] ),
    then it will replace that call with one of its arguments that satisfies the predicate [select_arg]. *)
let choose_fct (select_arg : var list -> int) : target -> unit =
  Target.apply_at_target_paths (Specialize_core.choose_on select_arg)

(** [choose_id id tg]: chooses the id of the arguments of the function [CHOOSE], then this id is used
    by the function [choose_fct]. *)
let choose_id (id : int) : target -> unit =
  choose_fct (fun _xs -> id)

(** [choose choice tg]: combines [choose_fct] and [choose_id] into one function so that [choice] is used
    when applying function [choose_fct]. *)
let choose (choice : var) (tg : target) : unit =
  choose_fct (fun xs ->
    match List.index_of choice xs with
    | None -> failwith "choose: the argument is not part of the choices"
    | Some id -> id) tg

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