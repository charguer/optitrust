open Ast
open Target

(* [unfold_let_mult_aux t]: transform the multiple variable declarations instruction to a sequence of variable declaration.
    DOES NOT WORK : cause different variable encoding between Trm_let, Trm_let_mult and function's arguments.
    Here some examples below. Also see the test file apac_unfold_let_mult.cpp
    Example : int i; int a = i, b = 1;
    Example : int a, *b = &a, *c = b;
    Example : void f(int i) { int a = i, int b = 1; } *)
let unfold_let_mult_aux (t : trm) : trm =
  match t.desc with
  | Trm_let_mult (vk, tvl, tl) ->
    let decls = List.map2 (fun (x, ty) t ->
      let t = match t.desc with
      | Trm_val _ | Trm_array _ -> t | _ -> trm_get t in
      if is_typ_const ty then trm_let_immut (x, get_inner_const_type (ty)) t else trm_let_mut (x, ty) t
    ) tvl tl in
    trm_seq_no_brace decls
  | _ -> fail None "Apac_core.unfold_let_mult: expected a target to a multiple variable declaration."

(* DOES NOT WORK : cause different variable encoding between Trm_let, Trm_let_mult and function's arguments *)
(* [unfold_let_mult t p]: applies [unfold_let_mult_aux] at the trm [t] with path [p]. *)
let unfold_let_mult : Transfo.local =
  apply_on_path(unfold_let_mult_aux)

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
