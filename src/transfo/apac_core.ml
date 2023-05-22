open Ast
open Target

(* [array_typ_to_ptr_typ]: change Typ_array to Typ_ptr {ptr_kind = Ptr_kind_mut; _} *)
let array_typ_to_ptr_typ (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_array (ty, _) -> typ_ptr Ptr_kind_mut ty
  | _ -> ty

(* [stack_to_heap_aux t]: transforms the variable declaration in such a way that
      the variable declaration declares on the heap.
    [t] - ast of the variable declaration. *)
let stack_to_heap_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (var, ty), tr, _) ->
    if trm_has_cstyle Reference t
      then begin match vk with
        | Var_immutable -> fail None "So reference are not always mutable."
          (* trm_let_immut (var, (typ_ptr Ptr_kind_mut ty)) (trm_new ty (trm_get tr)) *)
        | Var_mutable ->
          let in_typ = get_inner_ptr_type ty in
          if is_typ_const in_typ
            then trm_let_immut (var, ty) (trm_new in_typ (trm_get tr))
            else trm_let_mut (var, ty) (trm_new in_typ (trm_get tr))
        end
      else
        begin match vk with
        | Var_immutable -> trm_let_immut (var, (typ_ptr Ptr_kind_mut ty)) (trm_new ty tr)
        | Var_mutable ->
          let in_ty = get_inner_ptr_type ty in
          let ty = if is_typ_array in_ty then array_typ_to_ptr_typ in_ty else ty in
          let tr = if is_typ_array in_ty && is_typ_const (get_inner_array_type in_ty)
            then trm_new in_ty tr else tr in
          trm_let_mut (var, ty) tr
        end
  | Trm_let_mult (vk, tvl, tl) ->
    let l = List.map2 (fun (var, ty) t ->
      let ty2 =
        if is_typ_array ty then array_typ_to_ptr_typ ty
        else if is_typ_const ty then typ_const (typ_ptr Ptr_kind_mut ty)
        else typ_ptr Ptr_kind_mut ty
      in
      ((var, ty2), trm_new ty t)
      ) tvl tl in
    let (tvl, tl) = List.split l in
    trm_let_mult vk tvl tl
  | _ -> fail None "Apac_core.stack_to_heap: expected a target to a variable declaration."

(* [stack_to_heap is_const t p]: applies [stack_to_heap_aux] at the trm [t] with path [p]. *)
let stack_to_heap : Transfo.local =
  apply_on_path(stack_to_heap_aux)

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
