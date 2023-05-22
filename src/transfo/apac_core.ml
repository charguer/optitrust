open Ast
open Target

(* [mark_taskable_function]: add the mark [mark] to where tasks will be created in the body of the function.
    Naive implementation, minimum 2 function call in the body. *)
let mark_taskable_function_aux (mark : mark) (t :trm) : trm =
  let count = ref 0 in
  let rec aux (t : trm) : unit =
    match t.desc with
    | Trm_apps ({ desc = Trm_var _}, _) -> count := !count + 1; trm_iter aux t
    | _ -> trm_iter aux t
  in

  match t.desc with
  | Trm_let_fun (_, _, _, body, _) ->
    aux body;
    if !count >= 2 then trm_add_mark mark t else t
  | _ -> fail None "Apac_core.mark_taskable_function: expected a target to a function definition"

(* [mark_taskable_function t p]: applies [mark_taskable_function_aux] at the trm [t] with path [p]. *)
let mark_taskable_function (mark : mark) : Transfo.local =
  apply_on_path (mark_taskable_function_aux mark)
