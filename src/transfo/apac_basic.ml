open Ast
open Target
open Path

(* [insert_task_pragma_on t]: puts the instruction sequence of a taskifiable
    function's body into an OpenMP task group, i.e. a block of instructions
    delimited by curly brackets and prepended with a set of dedicated OpenMP
    pragmas. When applied on the 'main' function, the transformation creates the
    task group using the following pragmas:

      #pragma omp parallel
      #pragma omp master
      #pragma omp taskgroup

    Example:

      int main() {
        int a;
        f();
        return 0;
      }

      becomes

      int main() {
      #pragma omp parallel
      #pragma omp master
      #pragma omp taskgroup
      {
        int a;
        f()
      }
        return 0;
      }

    When applied to any other function except the 'main' function, the
    transformation creates the task group using the following pragma:

      #pragma omp taskgroup

    Example:

      int g() {
        int a;
        f();
        return 0;
      }

      becomes

      int g() {
      #pragma omp taskgroup
      {
        int a;
        f()
      }
        return 0;
      }
    [t] - AST of a taskifiable function definition.
*)
let insert_task_pragma_on (t : trm) : trm =
  let error_let =
    "Apac_basic.insert_task_pragma: expected a target to a function \
    definition" in
  (* Deconstruct the target function definition term [t]. *)
  let (qvar, ret_typ, args, body) =
    trm_inv ~error:error_let trm_let_fun_inv t in
  let error_seq =
    "Apac_basic.insert_task_pragma: expected body of a function definition" in
  (* Deconstruct the instruction sequence of the function's body. *)
  let body_seq = trm_inv ~error:error_seq trm_seq_inv body in
  (* Iterate over the instructions of the sequence and construct the updated
     function's body. *)
  let body_seq' = Mlist.map(fun item ->
    (* If an item of the body sequence is a sequence term, put it into an OpenMP
       task group by adding the necessary pragmas. *)
    if is_trm_seq item then
      let pragmas = if qvar.qvar_str = "main" then
                    [Parallel []; Master ; Taskgroup] else
                    [Taskgroup] in
      trm_add_pragmas pragmas item else
    (* Otherwise, keep the item as it is. *)
    item
  ) body_seq in
  (* Reconstruct the function definition with the updated body instruction
     sequence. *)
  trm_let_fun ~annot:t.annot ~qvar:qvar qvar.qvar_var ret_typ args
              (trm_seq body_seq')

(* [insert_task_pragma tg] applies insert_task_pragma_on. *)
let insert_task_pragma (tg : target) : unit =
  Target.apply_at_target_paths (insert_task_pragma_on) tg

(* [use_goto_for_return_aux mark t]: transforms the body of the funciton
      declaration in such a way that all return statements are replaced with
      gotos,
    [mark] - mark used to mark the introduced sequence.
    [t] - ast of the function definition. *)
let use_goto_for_return_aux (mark : mark) (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (qn, ret_ty, args, body) ->
    let seq_to_insert, _ = Internal.replace_return_with_assign ~check_terminal:false ~exit_label:"__exit" "__res" body in
    let seq_to_insert = trm_seq_add_last (trm_add_label "__exit" (trm_unit())) seq_to_insert in
    let new_body =
      begin match ret_ty.typ_desc with
      | Typ_unit ->
        trm_add_mark mark (trm_seq_nomarks [seq_to_insert;])
      | _ ->
        let new_decl = trm_let_mut ("__res", ret_ty) (trm_uninitialized ()) in
        trm_add_mark mark (trm_seq_nomarks [
          new_decl;
          seq_to_insert;
          trm_ret (Some (trm_var_get "__res"))
        ])
      end in
      trm_alter ~desc:(Trm_let_fun (qn, ret_ty, args, new_body)) t
  | _ -> fail t.loc "Apac_basic.use_goto_for_return_aux: expected a target to a function definition."

(* [use_goto_for_return mark]: expects the target [tg] to point at a function
    definition, then it will transform the body of that function definition as
    follows.

    First of all wraps the body of the function into a sequence and marks it
    with [mark] if [mark] <> "". Then it considers two cases.

    Case1:
      Function is of type void:
        1) Replaces each return statement inside the new sequence with
           goto __exit.
        2) After the wrapped sequence inserts an empty label "__exit".
    Case2:
      Function is returns a value of type [T] then:
        1) Inserts a declaration "T __res" just befor the introduced sequence.
        2) Replaces each return statement inside the wrapped sequence with
           "__res = x; goto __exit".
        3) Add after the new sequence, adds the labelled statement
           "__exit; return __res;". *)
let use_goto_for_return ?(mark : mark = "") (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_at_target_paths (use_goto_for_return_aux mark) tg
  )

(* [is_typdef_alias ty]: checks if [ty] is a defined type alias. *)
let is_typdef_alias (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) ->
      begin match td.typdef_body with
      | Typdef_alias _ -> true
      | _  -> false
      end
    | None -> false
    end
  | _ -> false

(* [get_inner_typdef_alias ty]: returns the inner type of the defined type
    alias. *)
let get_inner_typedef_alias (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) ->
      begin match td.typdef_body with
      | Typdef_alias ty -> Some (ty)
      | _  -> None
      end
    | None -> None
    end
  | _ -> None

(* [get_cptr_depth ty]: returns the number of C pointer of the type [ty]. *)
let get_cptr_depth (ty : typ) : int =
  let rec aux (depth : int) (ty : typ) : int =
    match ty.typ_desc with
    | Typ_const ty -> aux depth ty
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> aux (depth) ty
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } -> aux (depth+1) ty
    | Typ_array (ty, _) -> aux (depth+1) ty
    | Typ_constr _ when is_typdef_alias ty ->
      begin match get_inner_typedef_alias ty with
      | Some (ty) -> aux depth ty
      | None -> assert false
      end
    | _ -> depth
  in
  aux 0 ty

(* [get_constified_arg_aux ty]: return the constified typ of the typ [ty]*)
let rec get_constified_arg_aux (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with
  | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty} ->
    typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (get_constified_arg_aux ty))
  | Typ_const {typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty }; typ_annot = annot; typ_attributes = attributes} ->
    typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (get_constified_arg_aux ty))
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some td ->
      begin match td.typdef_body with
      | Typdef_alias ty -> get_constified_arg_aux ty
      | _ -> typ_const ty
      end
    | None -> typ_const ty
    end
  | Typ_const _ -> ty
  | _ -> typ_const ty

(* [get_constified_arg ty]: applies [get_constified_arg_aux] at the typ [ty] or
    the typ pointer by [ty].

    If [ty] is a reference or a rvalue reference, it returns the constified typ
    of [ty]. *)
let get_constified_arg (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
    begin match ty.typ_desc with
    (* rvalue reference *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
      typ_lref ~annot ~attributes (get_constified_arg_aux ty)
    (* reference *)
    | _ -> typ_ref ~annot ~attributes (get_constified_arg_aux ty)
    end
  | _ -> get_constified_arg_aux ty

(* [constify_args_aux is_args_const t]: transforms the type of arguments of a
    function declaration in such a way that "const" keywords are added wherever
    it is possible.

    [is_args_const] - list of booleans that tells for each argument whether it
     should be constified. The list should contain as many items as there are
     arguments in the target function.
    [t] - AST of the function definition. *)
let constify_args_aux (is_args_const : bool list) (is_method_const : bool) (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (qvar, ret_typ, args, body) ->
    let is_args_const = if is_args_const = []
      then List.init (List.length args) (fun _ -> true)
      else is_args_const in
    let const_args = (List.map2 (fun (v, ty) b ->
      if b then (v, (get_constified_arg ty)) else (v, ty)
      ) args is_args_const) in

    let t = trm_let_fun ~annot:t.annot ?loc:t.loc ?ctx:t.ctx ~qvar "" ret_typ const_args body in
    if is_method_const then trm_add_cstyle Const_method t else t
  | _ -> fail t.loc "Apac_basic.constify_args_aux expected a target to a function definition."
  
(* [constify_args ~is_const tg]: expect the target [tg] to point at a function
    definition. Then it will add the "const" keyword wherever it is possible in
    the type of the argument.

   The list [is_args_const] determines which argument to constify. *)
let constify_args ?(is_args_const : bool list = []) ?(is_method_const : bool = false): Transfo.t =
  Target.apply_at_target_paths (constify_args_aux is_args_const is_method_const)

(* [vars_tbl]: hashtable generic to keep track of variables and their pointer
    depth. This abstrastion is used for generic functions. *)
type 'a vars_tbl = (var, (int * 'a)) Hashtbl.t

(* [vars_arg]: hashtable of variables referring to the pointer depth of that
    argument and the position of the argument. *)
type vars_arg = int vars_tbl

(* [get_inner_all_unop_and_access t]: unfold all unary operators and array
    accesses. *)
let rec get_inner_all_unop_and_access (t : trm) : trm =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop _)); _ }, [t]) -> get_inner_all_unop_and_access t
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop array_access)); _ }, [t; _]) -> get_inner_all_unop_and_access t
  | _ -> t

(* [get_vars_data_from_cptr_arith va t] : resolve pointer operation to get the
    pointer variable. Then, return the data of the corresponding variable stored
    in vars_tbl. *)
let get_vars_data_from_cptr_arith (va : 'a vars_tbl) (t: trm) : 'a option =
  let rec aux (depth : int) (t: trm) : 'a option =
    match t.desc with
    (* unop : progress deeper + update depth *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _ }, [t]) ->
      begin match uo with
      | Unop_get -> aux (depth-1) t
      | Unop_address -> aux (depth+1) t
      | Unop_cast ty -> aux (depth + get_cptr_depth ty) t
      | _ -> None
      end
    (* binop array access : progress deeper + update depth *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _ },
        [t; _]) -> aux (depth-1) t
    (* binop : progress deeper + resolve left and right sides *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
      begin match (aux depth lhs, aux depth rhs) with
      | Some(res), None -> Some(res)
      | None, Some(res) -> Some(res)
      | None, None -> None
      | Some(_), Some(_) -> fail None "Should not happen : Binary operator between pointers"
      end
    (* variable : resolve variable *)
    | Trm_var (_ ,qv) ->
      begin match Hashtbl.find_opt va qv.qvar_str with
      | Some (d, arg_idx) when (d + depth) > 0 -> Some (arg_idx)
      | _ -> None
      end
    | _ -> None
  in
  aux 0 t

(* [update_vars_arg_on_trm_let on_ref on_ptr on_other va t]:
    It will add the variable to [va] if it is a reference or a pointer to an
    argument. Then, it will call the corresponding callback.

    [va] : vars_arg which stores the arguments,
    [t] : trm of a variable declaration,
    [on_ref] : callback if the variable is a reference to an argument,
    [on_ptr] : callback if the variable is a pointer to an argument,
    [on_other] : callback for the remaining cases. *)
let update_vars_arg_on_trm_let (on_ref : unit -> 'a) (on_ptr : unit -> 'a) (on_other : unit -> 'a) (va : vars_arg) (t: trm) : 'a =
  match t.desc with
  | Trm_let (_, (lname, ty), { desc = Trm_apps (_, [tr]); _ }) ->
    if trm_has_cstyle Reference t then
      match (get_inner_all_unop_and_access tr).desc with
      | Trm_var (_, qv) when Hashtbl.mem va qv.qvar_str ->
        let (_, arg_idx) = Hashtbl.find va qv.qvar_str in
        Hashtbl.add va lname (get_cptr_depth ty, arg_idx);
        on_ref()
      | _ -> on_other()
    else if is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
      match get_vars_data_from_cptr_arith va tr with
      | Some (arg_idx) ->
        Hashtbl.add va lname (get_cptr_depth ty, arg_idx);
        on_ptr()
      | None -> on_other()
    else on_other()
  | _ -> fail None "Apac_basic.update_vars_arg_on_trm_let: expect [t] to be a variable declaration"

(* [update_vars_arg_on_trm_let_mult_iter on_ref on_ptr on_other va t]:
      It will add the variable to [va] if it is a reference or a pointer to an argument,
      then it will call the corresponding callback.
    [va] : vars_arg which stores the arguments
    [name] : name of the variable
    [ty] : type of the variable
    [t] : trm of the right side of the "-" (rvalue),
    [on_ref] : callback if the variable is a reference to an argument,
    [on_ptr] : callback if the variable is a pointer to an argument,
    [on_other] : callback for the remaining cases. *)
let update_vars_arg_on_trm_let_mult_iter (on_ref : unit -> 'a) (on_ptr : unit -> 'a) (on_other : unit -> 'a)
    (va : vars_arg) (name : var) (ty : typ) (t: trm) : 'a =
  if is_reference ty then
    match (get_inner_all_unop_and_access t).desc with
    | Trm_var (_, qv) when Hashtbl.mem va qv.qvar_str ->
      let (_, arg_idx) = Hashtbl.find va qv.qvar_str in
      Hashtbl.add va name (get_cptr_depth ty, arg_idx);
      on_ref()
    | _ -> on_other()
  else if is_typ_ptr (get_inner_const_type ty) then
    match get_vars_data_from_cptr_arith va t with
    | Some (arg_idx) ->
      Hashtbl.add va name (get_cptr_depth ty, arg_idx);
      on_ptr()
    | None -> on_other()
  else on_other()

(* [constify_args_alias_aux is_args_const t]: transforms the type of variables
    that refer to constified arguments in such way that "const" keywords are
    added wherever it is possible.

    Note : It will fail if it has to partially constify a Trm_let_mult.

    [is_args_const] - list of booleans that tells whether an argument is
     constified. Its length must be the number of arguments,
    [t] - AST of the function definition. *)
let constify_args_alias_aux (is_args_const : bool list) (t : trm) : trm =
  let rec aux (va : vars_arg) (t :trm) : trm=
    match t.desc with
    (* new scope *)
    | Trm_seq _ | Trm_for _ | Trm_for_c _ -> trm_map (aux (Hashtbl.copy va)) t
    (* the syntax allows to declare variable in the condition statement
       but clangml currently cannot parse it *)
    | Trm_if _ | Trm_switch _ | Trm_while _ -> trm_map (aux (Hashtbl.copy va)) t
    | Trm_let (_, (lname, ty), { desc = Trm_apps (_, [tr]); _ }) ->
      update_vars_arg_on_trm_let
        (fun () -> let ty = typ_ref (get_constified_arg (get_inner_ptr_type ty)) in trm_let_mut (lname, ty) tr)
        (fun () -> let ty = get_constified_arg (get_inner_ptr_type ty) in trm_let_mut (lname, get_inner_const_type ty) tr)
        (fun () -> t)
        va t
    | Trm_let_mult (vk, tvl, tl) ->
      (* fail if partial constify : more than zero, less than all *)
      let is_mutated = ref false in
      let l = List.map2 (fun (lname, ty) t ->
        update_vars_arg_on_trm_let_mult_iter
          (fun () -> is_mutated := true; ((lname, get_constified_arg ty), t))
          (fun () -> is_mutated := true; ((lname, get_constified_arg ty), t))
          (fun () -> if !is_mutated then fail None "Apac_basic.constify_args_alias_aux: Trm_let_mult partial constify" else ((lname, ty), t))
          va lname ty t
      ) tvl tl in
      let (tvl, tl) = List.split l in
      trm_let_mult vk tvl tl
    | _ -> trm_map (aux va) t
  in
  match t.desc with
  | Trm_let_fun (qvar, ret_typ, args, body) ->
    let is_const = if is_args_const = [] then List.init (List.length args) (fun _ -> true) else is_args_const in
    let va : vars_arg= Hashtbl.create 10 in
    (* Only add constified arguments *)
    List.iteri (fun i (b, (var, ty)) -> if b then Hashtbl.add va var (get_cptr_depth ty, i)) (List.combine is_const args);
    trm_let_fun ~annot:t.annot ?loc:t.loc ?ctx:t.ctx ~qvar "" ret_typ args (aux va body)
  | _ -> fail t.loc "Apac_basic.constify_args expected a target to a function definition."

(* [constify_args ~is_args_const tg]: expects target [tg] to point at a function
    definition. Then, in the body, it will add the "const" keyword wherever it
    is possible in variables that are pointers or references to the constified
    arguments of the function.

    The list [is_args_const] determines which argument is constified. *)
let constify_args_alias ?(is_args_const : bool list = []) : Transfo.t =
  Target.apply_at_target_paths (constify_args_alias_aux is_args_const)

(* [array_typ_to_ptr_typ]: changes Typ_array to
    Typ_ptr {ptr_kind = Ptr_kind_mut; _}. *)
let array_typ_to_ptr_typ (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_array (ty, _) -> typ_ptr Ptr_kind_mut ty
  | _ -> ty

(* [stack_to_heap_aux t]: transforms a variable declaration in such a way that
    the variable is declared on the heap.

   [t] - AST of the variable declaration. *)
let stack_to_heap_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (var, ty), tr) ->
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
  | _ -> fail None "Apac_basic.stack_to_heap: expected a target to a variable declaration."

(* [stack_to_heap tg]: expects the target [tg] to point at a variable
    declaration. Then, the variable will be declared on the heap. *)
let stack_to_heap : Transfo.t =
  Target.apply_at_target_paths (stack_to_heap_aux)

(* [unfold_let_mult_aux t]: transforms multiple variable declaration instruction
    to a sequence of variable declarations.

    DOES NOT WORK : causes different variable encoding between Trm_let,
    Trm_let_mult and function's arguments. Here some examples:

    Example : int i; int a = i, b = 1;
    Example : int a, *b = &a, *c = b;
    Example : void f(int i) { int a = i, int b = 1; }.

    Also see the test file apac_unfold_let_mult.cpp. *)
let unfold_let_mult_aux (t : trm) : trm =
  match t.desc with
  | Trm_let_mult (vk, tvl, tl) ->
    let decls = List.map2 (fun (x, ty) t ->
      let t = match t.desc with
      | Trm_val _ | Trm_array _ -> t | _ -> trm_get t in
      if is_typ_const ty then trm_let_immut (x, get_inner_const_type (ty)) t else trm_let_mut (x, ty) t
    ) tvl tl in
    trm_seq_no_brace decls
  | _ -> fail None "Apac_basic.unfold_let_mult: expected a target to a multiple variable declaration."

(* [unfold_let_mult tg]: expects target [tg] to point at a multiple variable
    declaration. Then, it will replace it by a sequence of simple variable
    declarations.

    DOES NOT WORK : causes different variable encoding between Trm_let,
    Trm_let_mult and function's arguments. *)
let unfold_let_mult (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_at_target_paths (unfold_let_mult_aux) tg)

(* [mark_taskable_function]: adds mark [mark] where tasks will be created in a
    function body. Naive implementation, minimum 2 function call in the body. *)
let mark_taskable_function_aux (mark : mark) (t :trm) : trm =
  let count = ref 0 in
  let rec aux (t : trm) : unit =
    match t.desc with
    | Trm_apps ({ desc = Trm_var _}, _) -> count := !count + 1; trm_iter aux t
    | _ -> trm_iter aux t
  in
  match t.desc with
  | Trm_let_fun (_, _, _, body) ->
    aux body;
    if !count >= 2 then trm_add_mark mark t else t
  | _ -> fail None "Apac_basic.mark_taskable_function: expected a target to a function definition"

(* [mark_taskable_function]: expects target [tg] to point at a function
    definition. Then, it may add mark [mark] if the function is taskable. *)
let mark_taskable_function (mark : mark) : Transfo.t =
  Target.apply_at_target_paths (mark_taskable_function_aux mark)