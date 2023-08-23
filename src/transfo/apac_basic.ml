open Syntax
open Target
open Path
include Apac_core

(* [mark_taskification_candidates_on]: see [mark_taskification_candidates]. *)
let mark_taskification_candidates_on (t : trm) : trm =
  (* Initialize a reference holding the number of function calls within the body
     of the target function definiton. *)
  let count = ref 0 in
  (* Define an auxiliary function to count the number of function calls within
     the body of the target function definition. *)
  let rec aux (t : trm) : unit =
    match t.desc with
    (* If [t] is a function call, increase [count] and recurse deeper in the
       AST. *)
    | Trm_apps ({ desc = Trm_var _}, _) -> incr count; trm_iter aux t
    (* Otherwise, recurse deeper in the AST. *)
    | _ -> trm_iter aux t
  in
  (* Deconstruct the function definition term [t]. *)
  let error = "Apac_basic.mark_taskification_candidates_on: expected a target \
               to a function definition." in
  let (_, _, _, body) = trm_inv ~error trm_let_fun_inv t in
  (* Call the locally-defined auxiliary function to count the number of function
     calls within the body of [t]. *)
  aux body;
  (* If there are at least two function calls, mark the entire function as
     candidate for taskification. Otherwie, return the AST term unchanged. *)
  if !count > 1 then trm_add_mark "taskify" t else t

(* [mark_taskification_candidates]: expects the target [tg] to point at a
   function definition. It marks the functions to taskify, i.e. the functions to
   the body of which we shall insert tasks. For now, we use a naive algorithm to
   determine the candidates for taskification. We consider every function
   performing at least two function calls. *)
let mark_taskification_candidates (tg : target) : unit =
  Target.apply_at_target_paths (mark_taskification_candidates_on) tg

(* [task_group_on ~master t]: see [task_group] *)
let task_group_on ~(master : bool) (t : trm) : trm =
  (* Draw the list of pragmas to apply. *)
  let pragmas = if master then
                [Parallel []; Master ; Taskgroup] else
                [Taskgroup] in
  (* Apply the pragmas on the target instruction sequence. *)
  trm_add_pragmas pragmas t

(* [task_group ~master t]: puts the instruction sequence of a function's body
    into an OpenMP task group, i.e. a block of instructions delimited by curly
    brackets and prepended with the OpenMP pragma '#pragma omp taskgroup'.

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

    If [master] is true, the transformation creates a task group that will be
    executed only by one thread, the master thread, using the following pragmas:

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

    [master] - decides whether extra pragmas should be added for limiting the
               execution of the task group to only thread, the master thread;
    [t] - AST of a function body.
*)
let task_group ~(master : bool) (tg : target) : unit =
  Target.apply_at_target_paths (task_group_on ~master:master) tg

(* [use_goto_for_return_on mark t]: see [use_goto_for_return]. *)
let use_goto_for_return_on (mark : mark) (t : trm) : trm =
  (* Deconstruct the target function definition AST term. *)
  let error =
    "Apac_basic.use_goto_for_return_on: expected a target to a function \
     definition." in
  let (qvar, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Within the function's body, replace return statements with assignments to a
     return variable '__res' (if the return type is other than 'void') and
     gotos to an exiting label '__exit'. The result is a sequence.
     Note that both the return variable and the exiting label are defined in the
     upcoming steps. *)
  let res_var = new_var "__res" in
  let body', _ = Internal.replace_return_with_assign ~check_terminal:false
    ~exit_label:"__exit" res_var body in
  (* Add the '__exit' label at the end of the sequence. *)
  let body' = trm_seq_add_last (trm_add_label "__exit" (trm_unit())) body' in
  (* Mark the sequence with [mark]. *)
  let body' = trm_add_mark mark body' in
  (* If the function's return type is not 'void', we need to declare the return
     variable '__res' at the beginning of the sequence and return its value at
     the end of the sequence. *)
  let body' = if is_type_unit ret_ty then trm_seq_nomarks [
    body'
  ] else trm_seq_nomarks [
    (trm_let_mut (res_var, ret_ty) (trm_uninitialized ()));
    body';
    trm_ret (Some (trm_var_get res_var))
  ] in
  (* Reconstruct the function definition with the update body instruction
     sequence. *)
  trm_let_fun ~annot:t.annot qvar ret_ty args body'

(* [use_goto_for_return mark]: expects the target [tg] to point at a function
    definition. It replaces potentially multiple return statements by a single
    return statement at the end of the function definition through the usage of
    gotos.

    First of all, the transformation wraps the function's body into a sequence
    and marks it with [mark] if [mark] <> "". Then,

    if the function is of type 'void', it:
        1) replaces each return statement inside the new sequence with
           'goto __exit',
        2) appends an empty exiting label '__exit' to the sequence;
    if the function returns a value, it:
        1) preprends the declaration of a return variable '__res' to the
           sequence,
        2) replaces each return statement inside the sequence with
           '__res = x; goto __exit'.
        3) appends the final and unique labelled return statement
           '__exit; return __res;' to the sequence.

    [mark] - mark to put on the sequence the function's body is wrapped into,
    [tg] - target function definition AST term. *)
let use_goto_for_return ?(mark : mark = "") (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.apply_at_target_paths (use_goto_for_return_on mark) tg
  )

(* [constify_args_on ?force t]: see [constify_args]. *)
let constify_args_on ?(force = false) (t : trm) : trm =
  (* Try to deconstruct the target function definition term. *)
  let error = "Apac_basic.constify_args_on expected a target to a function \
               definition." in
  let (qvar, ret_typ, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Optionally, force the constification of all of the function's arguments as
     well as the constification of the function itself. *)
  if force then
    begin
      (* Constify all of the function's arguments. *)
      let const_args = List.map (fun (v, ty) -> (v, (typ_constify ty))) args in
      (* Rebuild the function definition term using the list of constified
         arguments and constify it. *)
      trm_add_cstyle Const_method (
          trm_let_fun ~annot:t.annot ~qvar qvar.qvar_var ret_typ const_args body
        )
    end
  (* Otherwise, have a look at the constification record of the function to find
     out which of its arguments should be constified, if any, and whether the
     function itself should be constified. *)
  else
    (* Gather the constification record of the function. *)
    let const_record = Hashtbl.find Apac_core.const_records qvar.qvar_str in
    (* Simultaneously loop over the list of function's arguments as well as over
       the list of argument constification records and constify the arguments
       that should be constified according to the corresponding constification
       record. *)
    let const_args = List.map2 (fun (v, ty) (cr : const_arg) ->
                         if cr.is_const then (v, (typ_constify ty)) else (v, ty)
                       ) args const_record.const_args in
    (* Rebuild the function definition term using the list of constified
       arguments. *)
    let let_fun_with_const_args =
      trm_let_fun ~annot:t.annot ~qvar qvar.qvar_var ret_typ const_args body in
    (* Constify the function too if the constification record says so. *)
    if const_record.is_const then
      trm_add_cstyle Const_method let_fun_with_const_args
    else
      let_fun_with_const_args

(* [constify_args ?force tg]: expects the target [tg] to point at a function
   definition. Then, based on the constification records in [const_records], it
   shall constify the arguments that should be constified. If the corresponding
   constification record says so, it shall also constify the target function
   itself.

   One can force, e.g. for testing purposes, the function to ignore the
   constification records and to constify all of the function's arguments as
   well as the function itself. *)
let constify_args ?(force = false) (tg : target) : unit =
  Target.apply_at_target_paths (constify_args_on ~force) tg

(* [constify_aliases_on ?force t]: see [constify_aliases]. *)
let constify_aliases_on ?(force = false) (t : trm) : trm =
  (* Auxiliary function to recursively constify all of the aliases. *)
  let rec aux (aliases : const_aliases) (t : trm) : trm =
    match t.desc with
    (* New scope: do nothing and recurse deeper with a local copy of the table
       of aliases. *)
    | Trm_seq _
      | Trm_for _
      | Trm_for_c _
      | Trm_if _
      | Trm_switch _
      | Trm_while _ -> trm_map (aux (Hashtbl.copy aliases)) t
    (* Variable declaration: update list of aliases and constify the lvalue if
       it is an alias to a constant variable. *)
    | Trm_let (_, lval, { desc = Trm_apps (_, [rval]); _ }, _) ->
       (* Check whether the declared variable is a reference. *)
       let reference = trm_has_cstyle Reference t in
       (* Check whether the declared variable is an alias to a function argument
          or a previously declared variable and return the alias type, i.e.
          reference (1), pointer (2). (0) means that the variable is not an
          alias. *)
       let which_alias = trm_let_update_aliases ~reference lval rval aliases in
       (* If the variable is an alias, we have to constify the variable and
          rebuild the corresponding declaration term. *)
       if which_alias > 0 then
         begin
           let (lval_var, lval_ty) = lval in
           (* The variable is a reference. *)
           if which_alias == 1 then
             begin
               let const_ty =
                 typ_ref (typ_constify (get_inner_ptr_type lval_ty)) in
               trm_let_mut (lval_var, const_ty) rval
             end
           (* The variable is a pointer. *)
           else if which_alias == 2 then
             begin
               let const_ty = typ_constify (get_inner_ptr_type lval_ty) in
               trm_let_mut (lval_var, get_inner_const_type const_ty) rval
             end
           else t
         end
       (* If the variable is not an alias, there is nothing to do, we return the
          term as is. *)
       else
         t
    (* Multiple variable declaration: update list of aliases and constify the
       lvalues if they are alias to constant variables. *)
    | Trm_let_mult (vk, lvals, rvals) ->
       (* Check whether the declared variables represent aliases to function
          arguments or previously declared variables and return the alias types
          in a list where (1) corresponds to a reference, (2) to a pointer and
          (0) means that the variable is not an alias. *)
       let which_aliases : int list =
         List.map2 (
             fun lval rval -> trm_let_update_aliases lval rval aliases
           ) lvals rvals in
       (* Compute the sum of values in [which_aliases]. *)
       let sum_aliases = List.fold_left (fun a b -> a + b) 0 which_aliases in
       (* If there is at least one alias in the multiple variable declaration,
          [sum_aliases] shall be non-zero. *)
       if sum_aliases > 0 then
         begin
           (* In this case, we have two possible cases. One, all of the
              variables are aliases and have to be consitifed. Two, only some of
              the variables are aliases and not all of them have to be
              constified.

              To determine whether at least one of the variables is not an
              alias, we check the positivity of all values in [which_aliases]
              and compute a logical AND. *)
           let all_const_aliases =
             List.fold_left (fun a b -> a && (b > 0)) true which_aliases in
           (* If the result is [true], all of the variables are aliases and have
              to be constified. *)
           if all_const_aliases then
             begin
               let const_lvals = List.map (fun (lval_var, lval_ty) ->
                                     (lval_var, typ_constify lval_ty)
                                   ) lvals in
               trm_let_mult vk const_lvals rvals
             end
           (* FIXME: The other case is not implemented yet. Indeed, to ensure a
              partial constification of a multiple variable declaration, i.e.
              when not all of the declared variables have to be constified, is
              legal, we would have to transform the corresponding [trm_let_mult]
              into a sequence of [trm_let], i.e. a sequence of simple variable
              declarations. However, this transformation function, see
              [apac_unfold_let_mult], is not working properly yet. *)
           else
             begin
               fail t.loc "Apac_basic.constify_aliases_on: partial \
                           consitification of a multiple variable declaration \
                           is not supported yet."
             end
         end
       else
         t
    (* Other cases: do nothing and recurse deeper. *)
    | _ -> trm_map (aux aliases) t
  in
  (* This is the main entry point where the auxiliary function is called from.

     Deconstruct the function definition term. *)
  let error = "Apac_basic.constify_aliases_on: expected a target to a function \
               definition." in
  let (qvar, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Gather the constification record of the function. *)
  let const_record = Hashtbl.find Apac_core.const_records qvar.qvar_str in
  (* Create an alias hash table. *)
  let aliases : const_aliases = Hashtbl.create 10 in
  (* Optionally, force the constification of all of the aliases by adding all of
     the function arguments into the hash table of aliases. *)
  if force then
    begin
      List.iteri (fun index (arg_var, arg_ty) ->
          Hashtbl.add aliases arg_var (typ_get_degree arg_ty, index)
        ) args
    end
  (* Otherwise, have a look at the constification record of the function to find
     out which of its arguments have been constified, if any. Then, add them to
     the hash table of aliases so as to constify all of their aliases as well.

     The principle adopted here is the opposite of [const_compute_one]. In the
     latter, hash tables of aliases are used to keep track of variables or
     arguments that must not be constified whereas, here, we use them to keep
     track of only those arguments that have been constified and their alias
     variables to which we have to propagate the constification process. *)
  else
    begin
      let index = ref 0 in
      List.iter2 (fun (arg_var, arg_ty) (arg_cr : const_arg) ->
          if arg_cr.is_const then
            Hashtbl.add aliases arg_var (typ_get_degree arg_ty, !index)
        ) args const_record.const_args
    end;
  (* Call the auxiliary function to constify the aliases in the body of the
     function. *)
  let body_with_const_aliases = aux aliases body in
  (* Rebuild and return the function definition term with the updated body. *)
  trm_let_fun ~annot:t.annot ~qvar
    qvar.qvar_var ret_ty args body_with_const_aliases

(* [constify_aliases ?force tg]: expects target the target [tg] to point at a
   function definition. Then, based on the constification records in
   [const_records], it shall constify the aliases to arguments that have been
   constified during the constification process.

   One can force, e.g. for testing purposes, the function to ignore the
   constification records and to constify the aliases to all of the function's
   arguments. *)
let constify_aliases ?(force = false) (tg : target) : unit =
  Target.apply_at_target_paths (constify_aliases_on ~force) tg

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

let unfold_let_mult_on (t : trm) : trm =
  let error = "Apac_basic.unfold_let_mult_on: expected a target to a multiple \
               variable declaration." in
  let _ = Debug_transfo.trm ~internal:true "let_mult_before" t in
  let (vk, tvs, tis) = trm_inv ~error trm_let_mult_inv t in
  let declarations = List.map2 (fun tv ti -> let out = trm_let vk tv ti in out) tvs tis in
  let nt = trm_seq_no_brace declarations in
  let _ = Debug_transfo.trm "seq_after" nt in
  nt

(* [unfold_let_mult tg]: expects target [tg] to point at a multiple variable
    declaration. Then, it will replace it by a sequence of simple variable
    declarations.

    DOES NOT WORK : causes different variable encoding between Trm_let,
    Trm_let_mult and function's arguments. *)
let unfold_let_mult (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    Target.apply_at_target_paths (unfold_let_mult_on) tg)

(* [vars_tbl]: hashtable generic to keep track of variables and their pointer
    depth. This abstrastion is used for generic functions. *)
type 'a vars_tbl = (var, (int * 'a)) Hashtbl.t
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
      | Unop_cast ty -> aux (depth + typ_get_degree ty) t
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
      begin match Hashtbl.find_opt va qv with
      | Some (d, arg_idx) when (d + depth) > 0 -> Some (arg_idx)
      | _ -> None
      end
    | _ -> None
  in
  aux 0 t
*)