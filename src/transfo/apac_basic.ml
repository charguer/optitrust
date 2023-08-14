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

(* [typ_constify ty]: constifies [ty] by applying the 'const' keyword wherever
   it is possible. *)
let typ_constify (ty : typ) : typ =
  (* Aliases for often referenced values *)
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  (* Auxiliary function to recursively constify [ty], e.g. 'int * a' becomes
     'const int * const a'. *)
  let rec aux (ty : typ) : typ =
    match ty.typ_desc with
    (* [ty] is a pointer. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } ->
       typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
    (* [ty] is a constant pointer. *)
    | Typ_const { typ_desc =
                    Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty };
                  typ_annot = annot;
                  typ_attributes = attributes } ->
       typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
    (* [ty] is a user-defined constructed type. *)
    | Typ_constr (_, id, _) ->
       begin match Context.typid_to_typedef id with
       (* [ty] is a 'typedef' declaration. *)
       | Some td ->
          begin match td.typdef_body with
          (* If the constructed type is an alias to another type, recurse. *) 
          | Typdef_alias ty -> aux ty
          (* Otherwise, constify the constructed type and return. *)
          | _ -> typ_const ty
          end
       (* [ty] is not a 'typedef' declaration. *)
       | None -> typ_const ty
       end
    (* [ty] is already a constant type. There is nothing to do. *)
    | Typ_const _ -> ty
    (* Deal with any other case. *)
    | _ -> typ_const ty
  in
  (* Here begins the main entry point of the function, from where the auxiliary
     function is called. *)
  match ty.typ_desc with
  (* [ty] is a reference. *)
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
    begin match ty.typ_desc with
    (* [ty] is an rvalue reference, i.e. '&&' is used. *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
      typ_lref ~annot ~attributes (aux ty)
    (* [ty] is a simple reference. *)
    | _ -> typ_ref ~annot ~attributes (aux ty)
    end
  (* [ty] is of any other type. *)
  | _ -> aux ty

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
    (* List of constification records for all the argument of the function. *)
    const_args : const_arg list;
    (* Tells whether the function can be constified. Note that this information
       is relevant only if the function is a class member method. *)
    mutable is_const : bool;
    (* Tells whether the return value is a pointer. *)
    is_ret_ptr : bool;
    (* Tells whether the return value is a reference. *)
    is_ret_ref : bool;
    (* Tells whether the function is a class member method. *)
    mutable is_class_method : bool;
}

(* [const_funs]: type for a hash table of [const_fun]. The keys are the names of
   the functions. *)
type const_funs = (string, const_fun) Hashtbl.t

(* Create our hash table of [const_fun] with an initial size of 10. The size of
   the table will grow automatically if needed. *)
let const_records : const_funs = Hashtbl.create 10

(* [constifiable]: hashtable storing constification records for all of the
    functions. *)
type constifiable = (string, (bool list * bool)) Hashtbl.t

(* FIXME: Only termporary. *)
let cstfbl : constifiable = Hashtbl.create 10

(* Create a stack of arguments that must not be constified. *)
let to_unconst : arg_id Stack.t = Stack.create ()
         
(* [trm_resolve_pointer_and_check_if_alias t]: tries to resolve pointer
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

(* [trm_let_update_aliases ?reference tv ti aliases]: checks whether the
   variable declaration specified by the typed variable [tv] and the
   initializaion term [ti] creates an alias to an already existing variable or
   function argument. If it is the case, it updates the alias hash table
   [aliases] accordingly and returns [1] if [tv] is a reference and [2] if [tv]
   is a pointer. Otherwise, it does nothing and returns [0].

   Note that when the function is applied on the elements of a simple variable
   declaration, i.e. a [trm_let], the [is_reference] function used to check
   whether the new variable is a reference has no effect. This is due to
   differences in representing simple [trm_let] and multiple [trm_let_mult]
   variable declarations in OptiTrust. In this case, the optional [reference]
   parameter can be used to ensure the test evaluates correctly. See a usage
   example in [const_compte_one]. *)
let trm_let_update_aliases ?(reference = false)
      (tv : typed_var) (ti : trm) (aliases : const_aliases) : int =
  let (v, ty) = tv in
  let error = "Apac_basic.trm_let_update_aliases: unable to retrieve qualified \
               name of a variable initialization term" in
  if is_reference ty || reference then
    begin
      let ti_var = trm_strip_accesses_and_references_and_get ti in
      if trm_is_var ti_var then
        begin
          let ti_qvar = trm_inv ~error trm_var_inv_qvar ti_var in
          if Hashtbl.mem aliases ti_qvar.qvar_str then
            begin
              let (_, ti_index) =
                Hashtbl.find aliases ti_qvar.qvar_str in
              Hashtbl.add aliases v (0, ti_index);
              1
            end
          else 0
        end
      else 0
    end
  else if is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
    begin
      let (alias_idx, ti_var) =
        match (trm_resolve_pointer_and_check_if_alias ti aliases) with
        | Some (idx, t) -> (idx, t)
        | None -> (-1, trm_unit ())
      in
      let _ = Printf.printf "alias_idx: %d\n" alias_idx in
      if alias_idx > -1 then
        begin
          let ti_qvar = trm_inv ~error trm_var_inv_qvar ti_var in
          let _ = Printf.printf "rval_qvar_str: %s\n" ti_qvar.qvar_str in
          Hashtbl.add aliases v (get_cptr_depth ty, alias_idx);
          2
        end
      else 0
    end
  else 0

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
      let pth = if List.length qvar.qvar_path > 0 then List.nth qvar.qvar_path 0 else "N/A" in 
      let _ = Printf.printf "Candidate : %s at path : %s\n" qvar.qvar_str pth in
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
          is_const = true;
          is_ret_ptr = is_typ_ptr ret_ty;
          is_ret_ref = is_typ_array ret_ty || is_typ_ref ret_ty;
          is_class_method = false;
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
     let _ = Printf.printf "Call to %s\n" name.qvar_str in
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
                 let _ = Printf.printf "alias: %s\n" arg_qvar.qvar_str in
                 if arg_const.is_ptr_or_ref then
                   begin
                     Printf.printf "arg is ptr or ref (%s, %d)\n" fun_name index;
                     arg_const.to_unconst_by_propagation <-
                       (fun_name, arg_index) ::
                         arg_const.to_unconst_by_propagation
                   end
               end
           end
       ) args;
     trm_iter (const_compute_one aliases fun_name) fun_body
  (* Variable declaration: update list of aliases. *)
  | Trm_let (_, lval, { desc = Trm_apps (_, [rval]); _ }, _) ->
     let _ =
       trm_let_update_aliases ~reference:(trm_has_cstyle Reference fun_body)
         lval rval aliases in
     trm_iter (const_compute_one aliases fun_name) fun_body
  (* Multiple variable declaration: update list of aliases. *)
  | Trm_let_mult (_, lvals, rvals) ->
     List.iter2 (
         fun lval rval -> let _ = trm_let_update_aliases lval rval aliases in ()
       ) lvals rvals;
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
          let (alias_idx, rval_var) =
            match (trm_resolve_pointer_and_check_if_alias rval aliases) with
            | Some (idx, t) -> (idx, t)
            | None -> (-1, trm_unit ())
          in
          if alias_idx > -1 && not lval_deref then
            let (lval_degree, _) = List.nth all_aliases 0 in
            Hashtbl.add aliases lval_name (lval_degree, alias_idx)
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
     if fun_const.is_ret_ref then
       begin
         (* If the return type of the function is a reference, there are two
            return term types we can possibly deal with: *)
         match (trm_var_inv ret) with
         (* 1) a variable term corresponding to the reference, *)
         | Some ret_var_name ->
            begin
              (* Propagate the unconstification to all previously aliased
                 arguments. *)
              let all_aliases = Hashtbl.find_all aliases ret_var_name in
              List.iter (fun (_, arg_index) ->
                  Stack.push (fun_name, arg_index) to_unconst
                ) all_aliases
            end
         (* 2) a function call with reference return type. In this case, there
               is nothing to do as the function call cannot be an alias to an
               argument. *)
         | _ -> ()
       end
     else if fun_const.is_ret_ptr then
       begin
         let (alias_idx, ret_var) =
           match (trm_resolve_pointer_and_check_if_alias ret aliases) with
           | Some (idx, t) -> (idx, t)
           | None -> (-1, trm_unit ())
         in
         if alias_idx > -1 then Stack.push (fun_name, alias_idx) to_unconst
       end;
     trm_iter (const_compute_one aliases fun_name) fun_body
  | _ -> trm_iter (const_compute_one aliases fun_name) fun_body

let rec unconst () : unit =
  match Stack.pop_opt to_unconst with
  | Some (fun_name, index) ->
     Printf.printf "Unconst on (%s, %d)...\n" fun_name index;
     let const_record = Hashtbl.find const_records fun_name in
     let nb_args = List.length const_record.const_args in
     if index < nb_args then
       begin
         let arg = List.nth const_record.const_args index in
         if arg.is_const then
           begin  
             arg.is_const <- false;
             List.iter (
                 fun element -> Stack.push element to_unconst
               ) arg.to_unconst_by_propagation;
           end;
       end
     (* When the function is a class member method and a class sibling is
        being modified within its body, the function must not be
        consitifed. *)
     else if const_record.is_class_method then const_record.is_const <- false;
     unconst ()
  | None -> ()

let find_parent_typedef_record (p : path) : trm option =
  let reversed = List.tl (List.rev p) in
  let rec aux (p : path) : trm option =
    match p with
    | e :: f -> 
       begin
         let tg = target_of_path (List.rev p) in (* FIXME : Optimize by passing non-reversed list as argument ? *)
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_typedef { typdef_body = Typdef_record _; _ } -> Some (t)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed

let const_compute_all : Transfo.t =
  Stack.clear to_unconst;
  Target.iter (fun trm path ->
      let error = "Apac_basic.const_compute_all: expected target to a function \
                   definition." in
      let (qvar, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv
                                         (get_trm_at_path path trm) in
      let nb_args = List.length args in
      let const_record = Hashtbl.find const_records qvar.qvar_str in
      let aliases : const_aliases = Hashtbl.create 10 in
      let class_siblings = match (find_parent_typedef_record path) with
        | Some (td) -> typedef_get_members td
        | None -> [] in
      if List.length class_siblings > 0 then
        begin
          const_record.is_class_method <- true;
          List.iteri (fun index (name, typ) ->
              Printf.printf "Class sibling for aliases : %s\n" name;
              Hashtbl.add aliases name (get_cptr_depth typ, nb_args + index)
            ) class_siblings
        end
      else const_record.is_const <- false;
      (* Add arguments to the list of aliases. *)
      List.iteri (fun index (name, typ) ->
        Printf.printf "Arg name for aliases : %s\n" name;
        Hashtbl.add aliases name (get_cptr_depth typ, index)
      ) args;
      (* Actually compute the dependencies of the function definition at [path]
         and fill [to_unconst]. *)
      const_compute_one aliases qvar.qvar_str body
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
    let const_record = Hashtbl.find const_records qvar.qvar_str in
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
let constify_args ?(force = false) : Transfo.t =
  Target.apply_at_target_paths (constify_args_on ~force)

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
  let const_record = Hashtbl.find const_records qvar.qvar_str in
  (* Create an alias hash table. *)
  let aliases : const_aliases = Hashtbl.create 10 in
  (* Optionally, force the constification of all of the aliases by adding all of
     the function arguments into the hash table of aliases. *)
  if force then
    begin
      List.iteri (fun index (arg_var, arg_ty) ->
          Hashtbl.add aliases arg_var (get_cptr_depth arg_ty, index)
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
            Hashtbl.add aliases arg_var (get_cptr_depth arg_ty, !index)
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
let constify_aliases ?(force = false) : Transfo.t =
  Target.apply_at_target_paths (constify_aliases_on ~force)

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
  Internal.nobrace_remove_after (fun _ ->
      Target.apply_at_target_paths (unfold_let_mult_on) tg)

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
