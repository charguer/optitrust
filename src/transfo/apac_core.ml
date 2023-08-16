open Ast
open Target
open Path

(*****************************)
(* PART I: TYPE DECLARATIONS *)
(* I.1 Constification        *)
(*****************************)

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

(******************************************************************)
(* PART II: DECLARATION AND/OR INITIALIZATION OF GLOBAL VARIABLES *)
(* II.1 Constification                                            *)
(******************************************************************)

(* Create our hash table of [const_fun] with an initial size of 10. The size of
   the table will grow automatically if needed. *)
let const_records : const_funs = Hashtbl.create 10

(* Create a stack of arguments that must not be constified. *)
let to_unconst : arg_id Stack.t = Stack.create ()

(******************************)
(* PART III: HELPER FUNCTIONS *)
(* III.1 General-purpose      *)
(******************************)

(* [typ_is_alias ty]: checks if [ty] is a user-defined alias to a basic type. *)
let typ_is_alias (ty : typ) : bool =
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

(* [typ_get_alias ty]: if [ty] is a user-defined alias to a basic type, it
   returns the latter. *)
let typ_get_alias (ty : typ) : typ option =
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


(* [typ_get_degree ty]: computes and returns the pointer degree of the type
   [ty]. For example, for [int ** a], it returns 2. *)
let typ_get_degree (ty : typ) : int =
  (* Auxiliary function to actually compute the degree. *)
  let rec aux (degree : int) (ty : typ) : int =
    match ty.typ_desc with
    (* If [ty] is a constant type or a reference, keep the current degree value
       and recurse. *)
    | Typ_const ty -> aux degree ty
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> aux degree ty
    (* If [ty] is a pointer or an array, increase the degree value and
       recurse. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } -> aux (degree + 1) ty
    | Typ_array (ty, _) -> aux (degree + 1) ty
    (* If [ty] is a constructed user-defined type and it is an alias to a basic
       type, resolve the latter and compute the degree. *)
    | Typ_constr _ when typ_is_alias ty ->
      begin match typ_get_alias ty with
      | Some (ty) -> aux degree ty
      | None -> fail None "Apac_core.typ_get_degree: unable to determine the \
                           basic type of a typedef alias."
      end
    | _ -> degree
  in
  (* Call the auxiliary function to compute the degree and hide the [degree]
     parameter to the outside world. *)
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

(************************)
(* III.2 Constification *)
(************************)

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

(* [trm_resolve_var_name_in_unop_or_array_access_and_get t]: tries to resolve
   the variable involved in a unary operation (++, --, & or get) or array
   access [t] and return its name. *)
let rec trm_resolve_var_name_in_unop_or_array_access_and_get (t : trm) :
          string option =
  match t.desc with
  (* Recurse over unary operations and array accesses. *)
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [term]) when
         (is_prefix_unary op || is_postfix_unary op) ->
     trm_resolve_var_name_in_unop_or_array_access_and_get term
  | Trm_apps ({
          desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address)));
          _}, [term]) -> trm_resolve_var_name_in_unop_or_array_access_and_get term
  | Trm_apps ({
          desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
          _}, [term; _]) -> trm_resolve_var_name_in_unop_or_array_access_and_get term
  (* We found the variable term, return the full name of the variable. *)
  | Trm_var (_, qvar) -> Some qvar.qvar_str
  | _ -> None

(* [trm_resolve_pointer_and_check_if_alias t aliases]: tries to resolve pointer
   operation [t] and checks in [aliases] whether the resulting pointer is an
   argument or an alias to an argument. In the end, it returns the term
   corresponding to the resulting pointer and if the latter is an argument or an
   alias to an argument, it returns the associated argument index. *)
let trm_resolve_pointer_and_check_if_alias
      (t : trm) (aliases : const_aliases) : (int * trm) option =
  let rec aux (degree : int) (t : trm) : (int * trm) option =
    match t.desc with
    (* Unary operation: strip, update degree and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       | Unop_get -> aux (degree - 1) t
       | Unop_address -> aux (degree + 1) t
       | Unop_cast ty -> aux (degree + typ_get_degree ty) t
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

(* [trm_let_update_aliases ?reference tv ti aliases]: checks in [aliases]
   whether the variable declaration specified by the typed variable [tv] and the
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
  (* Deconstruct the typed variable. *)
  let (v, ty) = tv in
  let error = "Apac_basic.trm_let_update_aliases: unable to retrieve qualified \
               name of a variable initialization term" in
  (* If we are working with a reference, *)
  if is_reference ty || reference then
    begin
      (* we need to go through the access and reference operations to obtain the
         AST term corresponding to the variable we might be aliasing. *)
      let ti_var = trm_strip_accesses_and_references_and_get ti in
      if trm_is_var ti_var then
        begin
          (* Once we have found the variable term, we deconstruct it, *)
          let ti_qvar = trm_inv ~error trm_var_inv_qvar ti_var in
          (* check whether it represents an alias to an existing variable or
             argument *)
          if Hashtbl.mem aliases ti_qvar.qvar_str then
            begin
              (* and if it is the case, create a new entry in [aliases] to keep
                 trace of it. *)
              let (_, ti_index) =
                Hashtbl.find aliases ti_qvar.qvar_str in
              Hashtbl.add aliases v (0, ti_index);
              1
            end
          else 0
        end
      else 0
    end
      (* If we are working with a pointer, *)
  else if is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
    begin
      (* we have to go through all of the potential pointer operations within
         the initialization term and try to determine if they lead to a pointer
         variable which aliases an existing variable or argument. *)
      let (alias_idx, ti_var) =
        match (trm_resolve_pointer_and_check_if_alias ti aliases) with
        | Some (idx, t) -> (idx, t)
        | None -> (-1, trm_unit ())
      in
      let _ = Printf.printf "alias_idx: %d\n" alias_idx in
      (* If it is the case, create a new entry in [aliases] to keep trace of
         it. *)
      if alias_idx > -1 then
        begin
          let ti_qvar = trm_inv ~error trm_var_inv_qvar ti_var in
          let _ = Printf.printf "rval_qvar_str: %s\n" ti_qvar.qvar_str in
          Hashtbl.add aliases v (typ_get_degree ty, alias_idx);
          2
        end
      else 0
    end
  else 0

(* [find_parent_typedef_record p]: goes back up the path [p] and returns the
   first term corresponding to a class or a structure definition, if any. We use
   this function to determine the parent class of a structure or a function in
   order to access to the member variables of that class or structure. *)
let find_parent_typedef_record (p : path) : trm option =
  (* We shall go back on our steps in the path, i.e. in the direction of the
     root of the AST, so we need to reverse [p]. *)
  let reversed = List.tl (List.rev p) in
  (* We use an auxiliary function in order to hide to the outside world the need
     for the path reversal. *)
  let rec aux (p : path) : trm option =
    (* The function simply goes recursively through the reversed [p] *)
    match p with
    | e :: f -> 
       begin
         (* and if it detects a class or a structure definition, it returns
            it. *)
         (* FIXME : Optimize by passing non-reversed list as argument ? *)
         let tg = target_of_path (List.rev p) in
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_typedef { typdef_body = Typdef_record _; _ } -> Some (t)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed

(* [const_unconst] pops out the elements from [to_unconst] (global variable, see
   Part II.1) one by one and propagates the unconstification through the
   concerned function constification records in [const_records] (global
   variable, see Part II.1). *)
let rec const_unconst () : unit =
  (* Pop out an element from [to_unconst]. *)
  match Stack.pop_opt to_unconst with
  | Some (fun_name, index) ->
     Printf.printf "Unconst on (%s, %d)...\n" fun_name index;
     (* Find the corresponding function constification record in
        [const_records]. *)
     let const_record = Hashtbl.find const_records fun_name in
     (* Determine the number of function arguments. *)
     let nb_args = List.length const_record.const_args in
     (* If the index of the argument we should unconstify corresponds to one of
        the function's arguments, i.e. its value is less then the number of
        argument, *)
     if index < nb_args then
       begin
         (* we gather the corresponding argument constification record. *)
         let arg = List.nth const_record.const_args index in
         (* Then, if needed, *)
         if arg.is_const then
           begin
             (* we unconstify the argument *)
             arg.is_const <- false;
             (* and push to [to_unconst] all of the function arguments that
                should be unconstified by propagation. In other terms, we need
                to follow the dependencies too. *)
             List.iter (
                 fun element -> Stack.push element to_unconst
               ) arg.to_unconst_by_propagation;
           end;
       end
     (* When the function is a class member method, i.e. when the value of the
        index stored in [to_unconst] is greater or equal to the total number of
        arguments, and a class sibling is being modified within its body, the
        function must not be consitifed. *)
     else if const_record.is_class_method then const_record.is_const <- false;
     (* Recurse. *)
     const_unconst ()
  (* When the stack is empty, stop and return. *)
  | None -> ()

(******************************************)
(* PART IV: CORE TRANSFORMATION FUNCTIONS *)
(* IV.1 Constification                    *)
(******************************************)

(* [const_lookup_candidates_on]: see [const_lookup_candidates]. *)
let const_lookup_candidates_on (t : trm) : unit =
  (* Deconstruct the function definition term. *)
  let error = "Apac_basic.const_lookup_candidates: expected a target to a \
               function definition!" in
  let (qvar, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
  let pth = if List.length qvar.qvar_path > 0 then List.nth qvar.qvar_path 0 else "N/A" in 
  let _ = Printf.printf "Candidate : %s at path : %s\n" qvar.qvar_str pth in
  (* Create an argument constification record for all of the function's
     arguments. *)
  let const_args = List.map (
                       fun (_, ty) -> {
                           is_ptr_or_ref =
                             is_typ_ptr ty || is_typ_ref ty ||
                               is_typ_array ty;
                           is_const = true;
                           to_unconst_by_propagation = [];
                     }) args in
  (* Create the constification record for the function itself *)
  let const : const_fun = {
      const_args = const_args;
      is_const = true;
      is_ret_ptr = is_typ_ptr ret_ty;
      is_ret_ref = is_typ_array ret_ty || is_typ_ref ret_ty;
      is_class_method = false;
    } in
  (* and add it to [const_records] (global variable, see Part II.1). *)
  Hashtbl.add const_records qvar.qvar_str const

(* [const_lookup_candidates]: expects the target [tg] to point at a function
   definition. It adds a new entry into [const_records] (global variable, see
   Part II.1) based on the information about the function. *)
let const_lookup_candidates (tg : target) : unit =
  Target.iter_at_target_paths (const_lookup_candidates_on) tg

(* [const_compute_on]: see [const_compute]. *)
let const_compute_on (p : path) (t : trm) : unit =
  (* Auxiliary function which recursively visits all the terms of the body
     [fun_body] of the function [fun_name] in order to resolve dependencies
     between arguments and aliases. *)
  let rec aux (aliases : const_aliases) (fun_name : string)
            (fun_body : trm) : unit =
  match fun_body.desc with
  (* New scope *)
  | Trm_seq _
    | Trm_for _
    | Trm_for_c _
    | Trm_if _
    | Trm_switch _
    | Trm_while _ ->
     trm_iter (aux (Hashtbl.copy aliases) fun_name) fun_body
  (* Function call: update dependencies. *)
  | Trm_apps ({ desc = Trm_var (_ , name); _ }, args) when
         Hashtbl.mem const_records name.qvar_str ->
     let _ = Printf.printf "Call to %s\n" name.qvar_str in
     (* Find the corresponding function constification record containing the
        constification records of all of the arguments. *)
     let fun_call_const = Hashtbl.find const_records name.qvar_str in
     let fun_args_const = fun_call_const.const_args in
     (* For each argument of the function call, we *)
     List.iteri (fun index arg ->
         (* go through the access and reference operations to obtain the AST
            term corresponding to the argument variable and *)
         let arg_var = trm_strip_accesses_and_references_and_get arg in
         if trm_is_var arg_var then
           begin
             let error = "Apac_basic.const_compute_on: unable to retrieve \
                          qualified name of an argument" in
             (* deconstruct the variable term. *)
             let arg_qvar = trm_inv ~error trm_var_inv_qvar arg_var in
             (* If the variable is an alias, we have to *)
             if Hashtbl.mem aliases arg_qvar.qvar_str then
               begin
                 (* determine the index of the argument it is aliasing, *)
                 let (_, arg_index) = Hashtbl.find aliases arg_qvar.qvar_str in
                 (* find the contification record of that argument and *)
                 let arg_const = List.nth fun_args_const index in
                 let _ = Printf.printf "alias: %s\n" arg_qvar.qvar_str in
                 (* if the latter is a pointer or a reference,  *)
                 if arg_const.is_ptr_or_ref then
                   begin
                     Printf.printf "arg is ptr or ref (%s, %d)\n" fun_name index;
                     (* we will have to unconstify it by propagation. *)
                     arg_const.to_unconst_by_propagation <-
                       (fun_name, arg_index) ::
                         arg_const.to_unconst_by_propagation
                   end
               end
           end
       ) args;
     trm_iter (aux aliases fun_name) fun_body
  (* Variable declaration: update list of aliases. *)
  | Trm_let (_, lval, { desc = Trm_apps (_, [rval]); _ }, _) ->
     let _ =
       trm_let_update_aliases ~reference:(trm_has_cstyle Reference fun_body)
         lval rval aliases in
     trm_iter (aux aliases fun_name) fun_body
  (* Multiple variable declaration: update list of aliases. *)
  | Trm_let_mult (_, lvals, rvals) ->
     List.iter2 (
         fun lval rval -> let _ = trm_let_update_aliases lval rval aliases in ()
       ) lvals rvals;
     trm_iter (aux aliases fun_name) fun_body
  (* Assignment or compound assignment: update the unconstification stack. *)
  | Trm_apps _ when is_set_operation fun_body ->
     let error = "Apac_basic.const_compute_on: expected set operation." in
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
     trm_iter (aux aliases fun_name) fun_body
  (* Increment or decrement unary operation: update the unconstification
     stack. *)
  | Trm_apps _ when trm_is_unop_inc_or_dec fun_body ->
     let error = "Apac_basic.const_compute_on: unable to retrieve variable \
                  name" in
     let var_name = trm_inv ~error
                      trm_resolve_var_name_in_unop_or_array_access_and_get
                      fun_body
     in
     (* Propagate the unconstification to all previously aliased arguments. *)
     let all_aliases = Hashtbl.find_all aliases var_name in
     List.iter (fun (_, arg_index) ->
         Stack.push (fun_name, arg_index) to_unconst
       ) all_aliases;
     trm_iter (aux aliases fun_name) fun_body
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
         (* If the return type of the function is a pointer, *)
     else if fun_const.is_ret_ptr then
       begin
          (* we have to go through all of the potential pointer operations
             within the return term and try to determine if they lead to a
             pointer variable which aliases an existing variable or argument. *)
         let (alias_idx, ret_var) =
           match (trm_resolve_pointer_and_check_if_alias ret aliases) with
           | Some (idx, t) -> (idx, t)
           | None -> (-1, trm_unit ())
         in
         (* If it is the case, we need it to be unconstified. *)
         if alias_idx > -1 then Stack.push (fun_name, alias_idx) to_unconst
       end;
     trm_iter (aux aliases fun_name) fun_body
  | _ -> trm_iter (aux aliases fun_name) fun_body
  in
  (* This is the main entry point of the function from where the auxiliary
     function shall be called. *)
  (* Deconstruct the target function definition term. *)
  let error = "Apac_basic.const_compute_all: expected target to a function \
               definition." in
  let (qvar, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Count the function's arguments. *)
  let nb_args = List.length args in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Hashtbl.find const_records qvar.qvar_str in
  (* Create a hash table for aliases to arguments of the function. *)
  let aliases : const_aliases = Hashtbl.create 10 in
  (* Try to find the parent class of the function. If any, get all the member
     variables of the class and *)
  let class_siblings = match (find_parent_typedef_record p) with
    | Some (td) -> typedef_get_members td
    | None -> [] in
  (* add them to the table of aliases as well as *)
  if List.length class_siblings > 0 then
    begin
      const_record.is_class_method <- true;
      List.iteri (fun index (name, typ) ->
          Printf.printf "Class sibling for aliases : %s\n" name;
          Hashtbl.add aliases name (typ_get_degree typ, nb_args + index)
        ) class_siblings
    end
  else const_record.is_const <- false;
  (* the arguments of the function itself. This is necessary in order to be able
     to identify possible aliases to these variables within the body of the
     function during the analysis using the auxiliary function defined above. *)
  List.iteri (fun index (name, typ) ->
      Printf.printf "Arg name for aliases : %s\n" name;
      Hashtbl.add aliases name (typ_get_degree typ, index)
    ) args;
  (* Actually compute the dependencies of the function definition at [path]
     and fill [to_unconst]. *)
  aux aliases qvar.qvar_str body

(* [const_compute]: expects the target [tg] to point at a function definition.
   It recurses over the body of the target function definition in order to
   figure out which function arguments and possible aliases to arguments should
   not be constified and adds them to the [to_unconst] stack (global variable,
   see Part II.1). *)
let const_compute (tg : target) : unit =
  Target.iter (fun t p -> const_compute_on p (get_trm_at_path p t)) tg
