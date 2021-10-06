open Ast
open Target


(*  [any array_index tg] expects [tg] to point to a variable occurrence with annotation
      Any. Then it will replace the current variable occurrence with [array_index]
*)
let any (array_index : var) : Target.Transfo.t =
  Target.apply_on_targets (Specialize_core.any array_index)



let choose_fct (select_arg : string list -> int) : Target.Transfo.t =
  Target.apply_on_targets (Specialize_core.choose select_arg)

let choose_id (id : int) : Target.Transfo.t =
  choose_fct (fun _xs -> id)

let choose (choice : string) (tg : target) : unit =
  choose_fct (fun xs -> 
    match Tools.index_of choice xs with 
    | None -> fail None "choose: the argument is not part of the choices"
    | Some id -> id) tg
  
