open Ast
open Target




(* [any e tg] expects the target [tg] to be pointing to a call to the function [ANY] then it will replace it with [e]*)

let any (e : trm) : Target.Transfo.t =
  Target.apply_on_targets (Specialize_core.any e)

(* [choose_fct select_arg] expects the target [tg] to point to a call to the function [CHOOSE] 
    which is used in the delocalize transformation (refer to Variable.delocalize).
    then it will replaces that call with one of its arguments which satisfies the predicate
    [select_arg].
*)
let choose_fct (select_arg : string list -> int) : Target.Transfo.t =
  Target.apply_on_targets (Specialize_core.choose select_arg)

(* [choose_id] choose the id of the arguments of the function [CHOOSE], then this id is used
    by the function [choose_fct].
*)
let choose_id (id : int) : Target.Transfo.t =
  choose_fct (fun _xs -> id)

(* [choose] combines [choose_fct] and [choose_id] into one function so that [choice] is used 
    when applying the function [choose_fct]
*)
let choose (choice : string) (tg : target) : unit =
  choose_fct (fun xs -> 
    match Tools.index_of choice xs with 
    | None -> fail None "choose: the argument is not part of the choices"
    | Some id -> id) tg
  
