open Ast
open Target
open Path

(* [get_inner_all_unop_and_access t]: unfold all unary operators and array
    accesses. *)
let rec get_inner_all_unop_and_access (t : trm) : trm =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop _)); _ }, [t]) -> get_inner_all_unop_and_access t
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop array_access)); _ }, [t; _]) -> get_inner_all_unop_and_access t
  | _ -> t

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

(* [trm_strip_accesses_and_references_and_get t]: strips [*t, &t, ...]
    recursively and returns [t]. *)
let rec trm_strip_accesses_and_references_and_get (t : trm) : trm =
  match t.desc with
  | Trm_apps
    ({ desc = Trm_val (Val_prim (Prim_unop _)); _ }, [t]) ->
    trm_strip_accesses_and_references_and_get t
  | Trm_apps
    ({ desc = Trm_val (Val_prim (Prim_binop array_access)); _ }, [t; _]) ->
    trm_strip_accesses_and_references_and_get t
  | _ -> t

let trm_resolve_binop_lval_name_and_get_with_deref (t : trm) :
      (string * bool) option =
  let rec aux (dereferenced : bool) (t : trm) : (string * bool) option =
    match t.desc with
    (* We have found the variable, return. *)
    | Trm_var (_, qvar) -> Some (qvar.qvar_str, dereferenced)
    (* [t] is an array access, which means that the operand was dereferenced.
       Continue resolution on the latter. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux true t
    (* [t] is a unary operation. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (op))); _ }, [t]) ->
       begin match op with
       (* A get operation, e.g. [*operand], as well as a structure access, e.g.
          [operand.member], both imply that the operand was dereferenced.
          Continue resolution on the latter. *)
       | Unop_get
         | Unop_struct_access _ -> aux true t
       (* A structure access through pointer, e.g. [operand->member], means that
          the operand was not dereferenced. We have finished resolving,
          return. *)
       | Unop_struct_get label -> Some (label, dereferenced)
       (* In case of another binary operation, do nothing and continue
          resolution on the operand. *)
       | _ -> aux dereferenced t
       end
    | _ -> None
  in
  aux false t

(* [trm_resolve_var_name_in_unop_inc_or_dec_and_get t]: tries to resolve the
   variable involved in a unary increment or decrement operation [t] and return
   its name. *)
let rec trm_resolve_var_name_in_unop_inc_or_dec_and_get (t : trm) :
          string option =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [term]) when
         (is_prefix_unary op || is_postfix_unary op) ->
     trm_resolve_var_name_in_unop_inc_or_dec_and_get term
  | Trm_apps ({
          desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address)));
          _}, [term]) -> trm_resolve_var_name_in_unop_inc_or_dec_and_get term
  | Trm_apps ({
          desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
          _}, [term; _]) -> trm_resolve_var_name_in_unop_inc_or_dec_and_get term
  | Trm_var (_, qvar) -> Some qvar.qvar_str
  | _ -> None

(*let lookup_const_candidates : Transfo.t =
  (* Create a stack for the arguments to unconstify. *)
  let to_process : arg_id Stack.t = Stack.create () in
  Target.iter (fun trm path ->

  )*)

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
  let body', _ = Internal.replace_return_with_assign ~check_terminal:false
    ~exit_label:"__exit" "__res" body in
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
    (trm_let_mut ("__res", ret_ty) (trm_uninitialized ()));
    body';
    trm_ret (Some (trm_var_get "__res"))
  ] in
  (* Reconstruct the function definition with the update body instruction
     sequence. *)
  trm_let_fun ~annot:t.annot ~qvar:qvar qvar.qvar_var ret_ty args body'

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
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_at_target_paths (use_goto_for_return_on mark) tg
  )

(* [arg_id] an argument identifier. Within the constification process, we refer
    to function arguments through the name of the associated function and their
    position in the list of arguments of the function's definition.

    For example, let us consider the following function definition:

      int multiply(int op1, int op2) { return op1 * op2; }

    The 'arg_id' corresponding to the argument 'op2' will look as follows.

      ("multiply", 1) *)
type arg_id = string * int

(* [const_arg]: an argument constification record. *)
type const_arg = {
  (* Tells whether the argument is a reference or a pointer. *)
  is_ptr_or_ref : bool;
  (* Tells whether the argument can be constified. *)
  mutable is_const : bool;
  (* If the argument is a reference or a pointer, i.e. when [is_ptr_or_ref] is
     true, and if the value in memory it refers or points to is modified within
     the body of the associated function, the argument must not be constified.
     
     To explain [to_unconst_by_propagation] let us consider a function [g]
     defined as follows:

       void g(int &val) { val += 4; }

     and a function [f] defined as follows:

       int f(int a, int b) { g(b); return a + b; }

     In [g], [val] is a reference and the referenced value is modified within
     the function's body. Therefore, the argument [val] must not be constified.
     However, [f] calls [g] and passes one of its arguments, i.e. [b], by
     reference to [g]. This way, the value referenced by [b] will be modified
     within [g]. We already know that [val] in [g] must not be constified.
     Because of this dependency, we must propagate this decision also to [b] in
     [f]. To achieve this and keep track of the dependency, we will add:

       (f, 1)

     to the [to_unconst_by_propagation] list in the [const_arg] record of [val]
     in [g]. See [arg_id] for more details on this data type. *)
  mutable to_unconst_by_propagation : arg_id list;
}

(* [const_aliases]: type for hash table of argument aliases. Pointers and
   references used within a function definition might be linked to the same data
   as the arguments of the function, i.e. they represent aliases to the
   function's arguments. Therefore, when we constify an argument, we must
   constify its aliases too. To keep trace of argument aliases, we use a hash
   table data type where the key is the name of the alias and the value is a
   pair of integers. The first integer gives the position of the aliased
   argument in the list of arguments of the concerned function declaration. The
   second integer gives the pointer degree of the alias, if the latter is a
   pointer, e.g. the pointer degree of [int ** tab] is 2. *)
type const_aliases = (string, (int * int)) Hashtbl.t

(* [const_fun]: a function constification record. *)
type const_fun = {
  (* List of constification records for all the argument of the function. When,
     the function is in fact a class method, we consider the corresponding
     object as an argument of the function too. In this case, the object is
     prepended to 'const_args'. *)
  const_args : const_arg list;
  (* Tells whether the return value is a pointer. *)
  is_ret_ptr : bool;
  (* Tells whether the return value is a reference. *)
  is_ret_ref : bool;
}

(* [const_funs]: type for a hash table of [const_fun]. The keys are the names of
   the functions. *)
type const_funs = (string, const_fun) Hashtbl.t

(* Create our hash table of [const_fun] with an initial size of 10. The size of
   the table will grow automatically if needed. *)
let const_records : const_funs = Hashtbl.create 10

(* FIXME: Only termporary. *)
let cstfbl = Hashtbl.create 10

(* Create a stack of arguments that must not be constified. *)
let to_unconst : arg_id Stack.t = Stack.create ()
         
(* [trm_resolve_pointer_and_check_if_alias t] : tries to resolve pointer
   operation [t] and checks whether the resulting pointer is an argument or an
   alias to an argument. In the end, it returns the term corresponding to the
   resulting pointer and if the latter is an argument or an alias to an
   argument, it returns the associated argument index. *)
let trm_resolve_pointer_and_check_if_alias
      (t : trm) (aliases : const_aliases) : (int * trm) option =
  let rec aux (degree : int) (t : trm) : (int * trm) option =
    match t.desc with
    (* Unary operation: strip, update degree and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       | Unop_get -> aux (degree - 1) t
       | Unop_address -> aux (degree + 1) t
       | Unop_cast ty -> aux (degree + get_cptr_depth ty) t
       | _ -> None
       end
    (* Array access: strip, update degree and recurse. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux (degree - 1) t
    (* Other binary operation: strip, update degree and recurse on both left and
       right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       begin match (aux degree lhs, aux degree rhs) with
       | Some (res), None -> Some (res)
       | None, Some (res) -> Some (res)
       | None, None
         (* In practice, binary operations between two pointers supported in
            C/C++ can not lead to a valid alias of one of them. *)
         | Some (_), Some (_) -> None
       end
    (* Variable: check if its an argument or an alias to an argument, then
       return the corresponding argument index and AST term. *)
    | Trm_var (_, qvar) ->
       begin match Hashtbl.find_opt aliases qvar.qvar_str with
       | Some (deg, idx) when (degree + deg) > 0 -> Some (idx, t)
       | _ -> None
       end
    | _ -> None
  in
  aux 0 t

(* [const_lookup_candidates]: expects the target [tg] to point at a function
   definition. It adds a new entry into 'const_records' based on the information
   about the function. *)
let const_lookup_candidates : Transfo.t =
  Hashtbl.clear const_records;
  Target.iter (fun trm path ->
      let error = "Apac_basic.const_lookup_candidates: expected a target to a \
                   function definition!" in
      let fun_def = get_trm_at_path path trm in
      let (qvar, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv fun_def in
      let _ = Printf.printf "Candidate : %s\n" qvar.qvar_str in
      let const_args = List.map (
                           fun (_, ty) -> {
                               is_ptr_or_ref =
                                 is_typ_ptr ty || is_typ_ref ty ||
                                   is_typ_array ty;
                               is_const = true;
                               to_unconst_by_propagation = [];
                         }) args in
      let const : const_fun = {
          const_args = const_args;
          is_ret_ptr = is_typ_ptr ret_ty;
          is_ret_ref = is_typ_array ret_ty;
        } in
      Hashtbl.add const_records qvar.qvar_str const
    )

(* [const_compute_one]: recursively visits all the terms of a function
   definition body in order to resolve dependencies between arguments *)
let rec const_compute_one (aliases : const_aliases) (fun_name : string)
          (fun_body : trm) : unit =
  match fun_body.desc with
  (* New scope *)
  | Trm_seq _
    | Trm_for _
    | Trm_for_c _
    | Trm_if _
    | Trm_switch _
    | Trm_while _ ->
     trm_iter (const_compute_one (Hashtbl.copy aliases) fun_name) fun_body
  (* Function call: update dependencies. *)
  | Trm_apps ({ desc = Trm_var (_ , name); _ }, args) when
         Hashtbl.mem const_records name.qvar_str ->
     let fun_call_const = Hashtbl.find const_records name.qvar_str in
     let fun_args_const = fun_call_const.const_args in
     List.iteri (fun index arg ->
         let arg_var = trm_strip_accesses_and_references_and_get arg in
         if trm_is_var arg_var then
           begin
             let error = "Apac_basic.const_compute_one: unable to retrieve \
                          qualified name of an argument" in
             let arg_qvar = trm_inv ~error trm_var_inv_qvar arg_var in
             if Hashtbl.mem aliases arg_qvar.qvar_str then
               begin
                 let (_, arg_index) = Hashtbl.find aliases arg_qvar.qvar_str in
                 let arg_const = List.nth fun_args_const index in
                 if arg_const.is_ptr_or_ref then
                   begin
                     arg_const.to_unconst_by_propagation <-
                       (fun_name, arg_index) ::
                         arg_const.to_unconst_by_propagation
                   end
               end
           end
       ) args;
     trm_iter (const_compute_one aliases fun_name) fun_body
  (* Variable declaration: update list of aliases. *)
  | Trm_let (_, (lval_name, lval_typ), { desc = Trm_apps (_, [rval]); _ }, _) ->
     if trm_has_cstyle Reference fun_body then
       begin
         let rval_var = trm_strip_accesses_and_references_and_get rval in
         if trm_is_var rval_var then
           begin
             let error = "Apac_basic.const_compute_one: unable to retrieve \
                          qualified name of an rvalue" in
             let rval_qvar = trm_inv ~error trm_var_inv_qvar rval_var in
             if Hashtbl.mem aliases rval_qvar.qvar_str then
               begin
                 let (_, rval_index) =
                   Hashtbl.find aliases rval_qvar.qvar_str in
                 Hashtbl.add aliases lval_name (0, rval_index)
               end
           end
       end
     else if
       is_typ_ptr (get_inner_const_type (get_inner_ptr_type lval_typ)) then
       begin
         let error = "Apac_basic.const_compute_one: unable to resolve \
                      pointer" in
         let (rval_degree, rval_var) =
           trm_inv ~error trm_resolve_pointer_and_get_with_degree rval in
         if trm_is_var rval_var then
           begin
             let error = "Apac_basic.const_compute_one: unable to retrieve \
                          qualified name of an rvalue" in
             let rval_qvar = trm_inv ~error trm_var_inv_qvar rval_var in
             if Hashtbl.mem aliases rval_qvar.qvar_str then
               begin
                 let (arg_degree, arg_index) =
                   Hashtbl.find aliases rval_qvar.qvar_str in
                 (* [lval] is an alias of [rval] only if both of them are
                    pointers. *)
                 if (rval_degree + arg_degree) > 0 then
                   begin
                     Hashtbl.add aliases
                       lval_name (get_cptr_depth lval_typ, arg_index)
                   end
               end
           end
       end;
     trm_iter (const_compute_one aliases fun_name) fun_body
  (* Assignment or compound assignment: update the unconstification stack. *)
  | Trm_apps _ when is_set_operation fun_body ->
     let error = "Apac_basic.const_compute_one: expected set operation." in
     let (lval, rval) = trm_inv ~error set_inv fun_body in
     begin
       match trm_resolve_binop_lval_name_and_get_with_deref lval with
       (* The lvalue has been modified by assignment, if it is an argument or an
          alias to an argument, we must not constify it. *)
       | Some (lval_name, lval_deref) when Hashtbl.mem aliases lval_name ->
          (* An alias of the same name may have been used multiple times to
             alias different memory locations.

             For example, in:

             L1: void f(int * a, int * b, int * c) {
             L2:   int * d = a;
             L3:   d = c; 
             L4:   *d = 1;
             L5: }
             
             [d] is declared as an alias to [a] on L2. The data pointed to by
             [a] is not modified within the function. On L3, [d] becomes an
             alias for [c]. Then, on L4, the data pointed to by [c] is modified
             through its alias [d]. Therefore, the analysis will conclude that
             nor [c] nor [d] should be constified. However, for [a] it will
             conclude that the argument can be constified as the data it is
             pointing to is never modified within the function. In the end, this
             will produce a compilation error as [a], which became const, is
             assigned to the non-const [d] on L2.

             In order to prevent this situation from happening, we must
             propagate the unconstification to previously aliased arguments too.
             As the [aliases] hash table stores all the values that were ever
             assigned to a given key, we only have to [find_all] of them and
             push them to the unconstification stack. *)
          let all_aliases = Hashtbl.find_all aliases lval_name in
          List.iter (fun (_, arg_index) ->
              Stack.push (fun_name, arg_index) to_unconst
            ) all_aliases;
          (* When an alias changes a target, i.e. when the lvalue variable was
             not dereferenced, we have to add a new entry into [aliases]. This
             happens, for example, on L3 in the aforementioned example. *)
          let error = "Apac_basic.const_compute_one: unable to resolve \
                       pointer" in
          let (_, rval_var) =
            trm_inv ~error trm_resolve_pointer_and_get_with_degree rval in
          if trm_is_var rval_var && not lval_deref then
            begin
             let error = "Apac_basic.const_compute_one: unable to retrieve \
                          qualified name of an rvalue" in
              let rval_qvar = trm_inv ~error trm_var_inv_qvar rval_var in
              if Hashtbl.mem aliases rval_qvar.qvar_str then
                begin
                  let (arg_degree, arg_index) =
                    Hashtbl.find aliases rval_qvar.qvar_str in
                  let (lval_degree, _) = List.nth all_aliases 1 in
                  Hashtbl.add aliases lval_name (lval_degree, arg_index)
                end
            end
       | _ -> ()
     end;
     trm_iter (const_compute_one aliases fun_name) fun_body
  (* Increment or decrement unary operation: update the unconstification
     stack. *)
  | Trm_apps _ when trm_is_unop_inc_or_dec fun_body ->
     let error = "Apac_basic.const_compute_one: unable to retrieve variable \
                  name" in
     let var_name = trm_inv ~error
                      trm_resolve_var_name_in_unop_inc_or_dec_and_get fun_body
     in
     (* Propagate the unconstification to all previously aliased arguments. *)
     let all_aliases = Hashtbl.find_all aliases var_name in
     List.iter (fun (_, arg_index) ->
         Stack.push (fun_name, arg_index) to_unconst
       ) all_aliases;
     trm_iter (const_compute_one aliases fun_name) fun_body
  (* Return statement: update the unconstification stack if the return value is
     a reference or a pointer. *)
  | Trm_abort (Ret (Some ret)) ->
     let fun_const = Hashtbl.find const_records fun_name in
     if fun_const.is_ret_ptr || fun_const.is_ret_ref then
       begin
         let error = "Apac_basic.const_compute_one: unable to retrieve name of \
                      return variable" in
         let ret_var_name = trm_inv ~error trm_var_inv ret in
         if Hashtbl.mem aliases ret_var_name then
           begin
             (* Propagate the unconstification to all previously aliased
                arguments. *)
             let all_aliases = Hashtbl.find_all aliases ret_var_name in
             List.iter (fun (_, arg_index) ->
                 Stack.push (fun_name, arg_index) to_unconst
               ) all_aliases;
           end
       end;
     trm_iter (const_compute_one aliases fun_name) fun_body
  | _ -> trm_iter (const_compute_one aliases fun_name) fun_body

let rec unconst () : unit =
    match Stack.pop_opt to_unconst with
    | Some (fun_name, index) ->
      let { const_args; _ } = Hashtbl.find const_records fun_name in
      let arg = List.nth const_args index in
      if arg.is_const then
      begin
        arg.is_const <- false;
        List.iter (
          fun element -> Stack.push element to_unconst
        ) arg.to_unconst_by_propagation
      end;
      unconst ()
    | None -> ()

let const_compute_all : Transfo.t =
  Stack.clear to_unconst;
  Target.iter (fun trm path ->
      let error = "Apac_basic.const_compute_dependencies_and_fill_to_unconst: \
                   expected target to a function definition." in
      let (qvar, ret_ty, args, body) =
        trm_inv ~error trm_let_fun_inv (get_trm_at_path path trm) in
      let aliases : const_aliases = Hashtbl.create 10 in
      (* TODO : Trouver comment obtenir la classe à laquelle appartient la
         fonction, si tel est le cas, et ajouter les membres de la classe à la
         liste des alias. *)
      (* Add arguments to the list of aliases. *)
      List.iteri (fun index (name, typ) ->
        Hashtbl.add aliases name (get_cptr_depth typ, index)
      ) args;
      (* Actually compute the dependencies of the function definition at [path]
         and fill [to_unconst]. *)
      const_compute_one aliases qvar.qvar_str body
    )

let unconst_and_const () : unit =
  unconst ();
  (* FIXME: To remove once all of the constification functions will be
     recoded. *)
  Hashtbl.iter (fun fun_loc { const_args; _ } ->
    let is_one_const = List.map (fun { is_const; _ } -> is_const) const_args in
    Hashtbl.add cstfbl fun_loc (is_one_const, false)
  ) const_records

(* [constify_args_aux is_args_const t]: transforms the type of arguments of a
    function declaration in such a way that "const" keywords are added wherever
    it is possible.

    [is_args_const] - list of booleans that tells for each argument whether it
     should be constified. The list should contain as many items as there are
     arguments in the target function.
    [t] - AST of the function definition. *)
let constify_args_aux (is_args_const : bool list) (is_method_const : bool) (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (qvar, ret_typ, args, body, _) ->
    let is_args_const = if is_args_const = []
      then List.init (List.length args) (fun _ -> true)
      else is_args_const in
    let const_args = (List.map2 (fun (v, ty) b ->
      if b then (v, (get_constified_arg ty)) else (v, ty)
      ) args is_args_const) in

    let t = trm_let_fun ~annot:t.annot ?loc:t.loc ~qvar "" ret_typ const_args body in
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
  | Trm_let (_, (lname, ty), { desc = Trm_apps (_, [tr]); _ }, _) ->
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
    | Trm_let (_, (lname, ty), { desc = Trm_apps (_, [tr]); _ }, _) ->
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
  | Trm_let_fun (qvar, ret_typ, args, body, _) ->
    let is_const = if is_args_const = [] then List.init (List.length args) (fun _ -> true) else is_args_const in
    let va : vars_arg= Hashtbl.create 10 in
    (* Only add constified arguments *)
    List.iteri (fun i (b, (var, ty)) -> if b then Hashtbl.add va var (get_cptr_depth ty, i)) (List.combine is_const args);
    trm_let_fun ~annot:t.annot ?loc:t.loc ~qvar "" ret_typ args (aux va body)
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
  | Trm_let_fun (_, _, _, body, _) ->
    aux body;
    if !count >= 2 then trm_add_mark mark t else t
  | _ -> fail None "Apac_basic.mark_taskable_function: expected a target to a function definition"

(* [mark_taskable_function]: expects target [tg] to point at a function
    definition. Then, it may add mark [mark] if the function is taskable. *)
let mark_taskable_function (mark : mark) : Transfo.t =
  Target.apply_at_target_paths (mark_taskable_function_aux mark)
