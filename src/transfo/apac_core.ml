open Ast
open Typ
open Trm
open Target
open Path

(*****************************)
(* PART I: TYPE DECLARATIONS *)
(* I.1 Constification        *)
(*****************************)

(* [VarHashtbl]: specific type of hash tables where the keys are of type
   [var]. *)
module VarHashtbl = Hashtbl.Make(Var)

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

       (f, b)

     to the [to_unconst_by_propagation] associative (function variable ->
     argument variable) list in the [const_arg] record of [val] in [g]. *)
  mutable to_unconst_by_propagation : (var * var) list;
}

(* [const_fun]: a function constification record. *)
type const_fun = {
    (* Associative (argument variable -> constification record) list of
       constification records for all the argument of the function. *)
    const_args : (var * const_arg) list;
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

(* [const_funs]: type for a hash table of [const_fun]. The keys are functions
   represented by terms of type [var]. *)
type const_funs = const_fun VarHashtbl.t

(* [const_aliases]: type for hash table of argument aliases.

   Pointers and references used within a function definition might be linked to
   the same data as the arguments of the function, i.e. they represent aliases
   to the function's arguments. Therefore, when we constify an argument, we must
   constify its aliase(s) too. To keep trace of argument aliases, we use a hash
   table data type where the key is the [var] of the alias and the value is a
   pair of a [var] and an [int]. The [var] element corresponds to the function
   argument being aliased. The [int] element gives the pointer degree of the
   alias, if the latter is a pointer, e.g. the pointer degree of [int ** tab] is
   2. *)
type const_aliases = (var * int) VarHashtbl.t

(* [const_unconst]: type of stack of function arguments that must not be
   constified.

   At the beginning of the constification process, we assume that every function
   argument in every function can be constified, see
   [build_constification_records]. Then, we perform an analysis of dependencies
   between function arguments and write operations involving the latter within
   the body of corresponding functions, see [identify_mutables]. When the
   analysis concludes that a function argument is written to, it shall modify
   the associated constification record so as to mark the argument as
   non-consitifiable. The same goes also for class methods modifying sibling
   class members. Such methods should not be consitified either. See [const_arg]
   and [const_fun] for more details. 

   However, the constification records are not modified directly during the
   analysis. Instead, we use a stack keeping trace of functions arguments and
   functions that shall be unconstified once the analysis terminates, see
   [to_unconst] below. The elements of the stack are pairs of the [var]
   identifying the target function and the [var] identifying its argument to
   unconstify. If the latter represents the [var] of the function, i.e. when
   both of the elements of the pair are equal [vars], it means that the function
   itself should be unconstified. *)
type const_unconst = (var * var) Stack.t

(******************************************************************)
(* PART II: DECLARATION AND/OR INITIALIZATION OF GLOBAL VARIABLES *)
(* II.1 Constification                                            *)
(******************************************************************)

(* Create our hash table of [const_fun] with an initial size of 10. The size of
   the table will grow automatically if needed. *)
let const_records : const_funs = VarHashtbl.create 10

(* Create a stack of arguments that must not be constified. See [const_unconst]
   for more details. *)
let to_unconst : const_unconst = Stack.create ()

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
      (var * bool) option =
  let rec aux (dereferenced : bool) (t : trm) : (var * bool) option =
    match t.desc with
    (* We have found the variable, return. *)
    | Trm_var (_, var) -> Some (var, dereferenced)
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
          the operand was not dereferenced. To finish finished resolving,
          iterate once more on [t]. *)
       | Unop_struct_get label -> aux dereferenced t
       (* In case of another binary operation, do nothing and continue
          resolution on the operand. *)
       | _ -> aux dereferenced t
       end
    | _ -> None
  in
  aux false t

(* [trm_resolve_var_name_in_unop_or_array_access_and_get t]: tries to resolve
   the variable involved in a unary operation (++, --, & or get) or array
   access [t] and return it. *)
let rec trm_resolve_var_name_in_unop_or_array_access_and_get (t : trm) :
          var option =
  match t.desc with
  (* Recurse over unary operations and array accesses. *)
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [term]) when
         (is_prefix_unary op || is_postfix_unary op) ->
     trm_resolve_var_name_in_unop_or_array_access_and_get term
  | Trm_apps ({
          desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address)));
          _}, [term]) ->
     trm_resolve_var_name_in_unop_or_array_access_and_get term
  | Trm_apps ({
          desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
          _}, [term; _]) ->
     trm_resolve_var_name_in_unop_or_array_access_and_get term
  (* We found the variable term, return the full name of the variable. *)
  | Trm_var (_, var) -> Some var
  | _ -> None

(* [trm_resolve_pointer_and_aliased_variable t aliases]: tries to resolve
   pointer operation [t] and checks in [aliases] whether the resulting pointer
   is an argument or an alias to an argument. If the pointer operation succeedes
   and if the resulting pointer is an argument or an alias to an argument, the
   function returns the variable corresponding to the resulting pointer as well
   as the aliased variable. *)
let trm_resolve_pointer_and_aliased_variable
      (t : trm) (aliases : const_aliases) : (var * var) option =
  let rec aux (degree : int) (t : trm) : (var * var) option =
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
    | Trm_var (_, v) ->
       begin match VarHashtbl.find_opt aliases v with
       | Some (alias, deg) when (degree + deg) > 0 -> Some (v, alias)
       | _ -> None
       end
    | _ -> None
  in
  aux 0 t

(* [trm_can_resolve_to_unique t]: tries to resolve operation [t] to unique variable and
   returns [true] on success and [false] otherwise. *)
(*
let rec trm_can_resolve_pointer (t : trm) : bool =
    match t.desc with
    (* Unary operation: strip, update degree and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       | Unop_get
         | Unop_address
         | Unop_cast _ -> aux t
       | _ -> false
       end
    (* Array access: strip, update degree and recurse. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux t
    (* Other binary operation: strip, update degree and recurse on both left and
       right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       begin match (aux lhs, aux rhs) with
       | true, false -> true
       | false, true -> true
       | false, false
         (* In practice, binary operations between two pointers supported in
            C/C++ can not lead to a valid alias of one of them. *)
         | true, true -> false
       end
    (* Variable: check if its an argument or an alias to an argument, then
       return the corresponding argument index and AST term. *)
    | Trm_var _ -> true
    | _ -> false
 *)
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
      let ti_trm_var = trm_strip_accesses_and_references_and_get ti in
      if trm_is_var ti_trm_var then
        begin
          (* Once we have found the variable term, we deconstruct it, *)
          let ti_var = trm_inv ~error trm_var_inv ti_trm_var in
          (* check whether it represents an alias to an existing variable or
             argument *)
          if VarHashtbl.mem aliases ti_var then
            begin
              (* and if it is the case, create a new entry in [aliases] to keep
                 trace of it. *)
              let (aliased, _) = VarHashtbl.find aliases ti_var in
              VarHashtbl.add aliases v (aliased, 0);
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
      match (trm_resolve_pointer_and_aliased_variable ti aliases) with
      (* If it is the case, create a new entry in [aliases] to keep trace of
         it. *)
      | Some (_, aliased) ->
         begin
           let _ = Printf.printf "rval is aliasing: %s\n" (var_to_string aliased) in
           VarHashtbl.add aliases v (aliased, typ_get_degree ty);
           2
         end
      | None -> 0
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

(* [unconstify_mutables] pops out the elements from [to_unconst] (global
   variable, see Part II.1) one by one and propagates the unconstification
   through the concerned function constification records in [const_records]
   (global variable, see Part II.1). *)
let rec unconstify_mutables () : unit =
  (* Pop out an element from [to_unconst]. *)
  match Stack.pop_opt to_unconst with
  | Some (fn, ar) ->
     Printf.printf "Unconst on (%s, %s)...\n" (var_to_string fn) (var_to_string ar);
     (* Find the corresponding function constification record in
        [const_records]. *)
     let const_record = VarHashtbl.find const_records fn in
     (* If the argument we should unconstify is in fact the function itself, *)
     if var_eq fn ar then
       (* it means that the function is a class member method and a class
          sibling is being modified within its body. Therefore, the function
          must not be consitifed. *)
       begin
         if const_record.is_class_method then const_record.is_const <- false
       end
     else
       begin
         (* we gather the corresponding argument constification record. *)
         let (_, arg) =
           List.find (fun (x, _) -> x.id = ar.id) const_record.const_args in
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
       end;
     (* Recurse. *)
     unconstify_mutables ()
  (* When the stack is empty, stop and return. *)
  | None -> ()

(******************************************)
(* PART IV: CORE TRANSFORMATION FUNCTIONS *)
(* IV.1 Constification                    *)
(******************************************)

(* [build_constification_records_on]: see [build_constification_records]. *)
let build_constification_records_on (t : trm) : unit =
  (* Deconstruct the function definition term. *)
  let error = "Apac_basic.const_lookup_candidates: expected a target to a \
               function definition!" in
  let (var, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
  let _ = Printf.printf "Candidate : %s\n" (var_to_string var) in
  (* Create an argument constification record for all of the function's
     arguments. *)
  let const_args = List.map (
                       fun (arg, ty) -> (arg, {
                           is_ptr_or_ref =
                             is_typ_ptr ty || is_typ_ref ty ||
                               is_typ_array ty;
                           is_const = true;
                           to_unconst_by_propagation = [];
                     })) args in
  (* Create the constification record for the function itself *)
  let const : const_fun = {
      const_args = const_args;
      is_const = true;
      is_ret_ptr = is_typ_ptr ret_ty;
      is_ret_ref = is_typ_array ret_ty || is_typ_ref ret_ty;
      is_class_method = false;
    } in
  (* and add it to [const_records] (global variable, see Part II.1) if it is not
     present in the hash table already, e.g. in the case of a
     pre-declaration. *)
  if not (VarHashtbl.mem const_records var) then
    begin
      Printf.printf "Accepted : %s\n" (var_to_string var);
      VarHashtbl.add const_records var const
    end
      

(* [build_constification_records]: expects the target [tg] to point at a
   function definition. It adds a new entry into [const_records] (global
   variable, see Part II.1) based on the information about the function. *)
let build_constification_records (tg : target) : unit =
  Target.iter_at_target_paths (build_constification_records_on) tg

(* [identify_mutables_on]: see [identify_mutables]. *)
let identify_mutables_on (p : path) (t : trm) : unit =
  (* Auxiliary function which recursively visits all the terms of the body
     [fun_body] of the function [fun_var] in order to resolve dependencies
     between arguments and aliases. *)
  let rec aux (aliases : const_aliases) (fun_var : var)
            (fun_body : trm) : unit =
    match fun_body.desc with
    (* New scope *)
    | Trm_seq _
      | Trm_for _
      | Trm_for_c _
      | Trm_if _
      | Trm_switch _
      | Trm_while _ ->
       trm_iter (aux aliases fun_var) fun_body
    (* Function call: update dependencies. *)
    | Trm_apps ({ desc = Trm_var (_ , name); _ }, args) when
           VarHashtbl.mem const_records name ->
       let _ = Printf.printf "Call to %s\n" (var_to_string name) in
       (* Find the corresponding function constification record containing the
          constification records of all of the arguments. *)
       let fun_call_const = VarHashtbl.find const_records name in
       let fun_args_const = fun_call_const.const_args in
       (* For each argument of the function call, we *)
       List.iteri (fun index arg ->
           (* go through the access and reference operations to obtain the AST
              term corresponding to the argument variable and *)
           let arg_trm_var = trm_strip_accesses_and_references_and_get arg in
           if trm_is_var arg_trm_var then
             begin
               let error = "Apac_basic.identify_mutables_on: unable to \
                            retrieve qualified name of an argument" in
               (* deconstruct the variable term. *)
               let arg_var = trm_inv ~error trm_var_inv arg_trm_var in
               (* If the variable is an alias, we have to *)
               if VarHashtbl.mem aliases arg_var then
                 begin
                   (* determine the index of the argument it is aliasing, *)
                   let (aliased, _) = VarHashtbl.find aliases arg_var in
                   (* find the contification record of that argument and *)
                   let (_, arg_const) = List.nth fun_args_const index in
                   let _ = Printf.printf "alias: %s\n" (var_to_string arg_var) in
                   (* if the latter is a pointer or a reference,  *)
                   if arg_const.is_ptr_or_ref then
                     begin
                       Printf.printf "arg is ptr or ref (%s, %s)\n" (var_to_string fun_var) (var_to_string aliased);
                       (* we will have to unconstify it by propagation. *)
                       arg_const.to_unconst_by_propagation <-
                         (fun_var, aliased) ::
                           arg_const.to_unconst_by_propagation
                     end
                 end
             end
         ) args;
       trm_iter (aux aliases fun_var) fun_body
    (* Variable declaration: update list of aliases. *)
    | Trm_let (_, lval, { desc = Trm_apps (_, [rval]); _ }, _) ->
       let _ =
         trm_let_update_aliases ~reference:(trm_has_cstyle Reference fun_body)
           lval rval aliases in
       trm_iter (aux aliases fun_var) fun_body
    (* Multiple variable declaration: update list of aliases. *)
    | Trm_let_mult (_, lvals, rvals) ->
       List.iter2 (
           fun lval rval ->
           let _ = trm_let_update_aliases lval rval aliases in ()
         ) lvals rvals;
       trm_iter (aux aliases fun_var) fun_body
    (* Assignment or compound assignment: update the unconstification stack. *)
    | Trm_apps _ when is_set_operation fun_body ->
       let error = "Apac_basic.identify_mutables_on: expected set operation." in
       let (lval, rval) = trm_inv ~error set_inv fun_body in
       begin
         match trm_resolve_binop_lval_name_and_get_with_deref lval with
         (* The lvalue has been modified by assignment, if it is an argument or
            an alias to an argument, we must not constify it. *)
         | Some (lval_var, lval_deref) when VarHashtbl.mem aliases lval_var ->
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
               alias for [c]. Then, on L4, the data pointed to by [c] is
               modified through its alias [d]. Therefore, the analysis will
               conclude that nor [c] nor [d] should be constified. However, for
               [a] it will conclude that the argument can be constified as the
               data it is pointing to is never modified within the function. In
               the end, this will produce a compilation error as [a], which
               became const, is assigned to the non-const [d] on L2.

               In order to prevent this situation from happening, we must
               propagate the unconstification to previously aliased arguments
               too. As the [aliases] hash table stores all the values that were
               ever assigned to a given key, we only have to [find_all] of them
               and push them to the unconstification stack. *)
            let all_aliases = VarHashtbl.find_all aliases lval_var in
            List.iter (fun (aliased, _) ->
                Stack.push (fun_var, aliased) to_unconst
              ) all_aliases;
            (* When an alias changes a target, i.e. when the lvalue variable was
               not dereferenced, we have to add a new entry into [aliases]. This
               happens, for example, on L3 in the aforementioned example. *)
            begin
              match (trm_resolve_pointer_and_aliased_variable rval aliases) with
              | Some (_, aliased) ->
                 if not lval_deref then
                   let (_, lval_degree) = List.nth all_aliases 0 in
                   VarHashtbl.add aliases lval_var (aliased, lval_degree)
              | None -> ()
            end
         | _ -> ()
       end;
       trm_iter (aux aliases fun_var) fun_body
    (* Increment or decrement unary operation: update the unconstification
       stack. *)
    | Trm_apps _ when trm_is_unop_inc_or_dec fun_body ->
       let error = "Apac_basic.identify_mutables_on: unable to retrieve
                    variable name" in
       let var_var = trm_inv ~error
                       trm_resolve_var_name_in_unop_or_array_access_and_get
                       fun_body
       in
       (* Propagate the unconstification to all previously aliased arguments. *)
       let all_aliases = VarHashtbl.find_all aliases var_var in
       List.iter (fun (aliased , _) ->
           Stack.push (fun_var, aliased) to_unconst
         ) all_aliases;
       trm_iter (aux aliases fun_var) fun_body
    (* Return statement: update the unconstification stack if the return value
       is a reference or a pointer. *)
    | Trm_abort (Ret (Some ret)) ->
       let fun_const = VarHashtbl.find const_records fun_var in
       if fun_const.is_ret_ref then
         begin
           (* If the return type of the function is a reference, there are two
              return term types we can possibly deal with: *)
           match (trm_var_inv ret) with
           (* 1) a variable term corresponding to the reference, *)
           | Some ret_var ->
              begin
                (* Propagate the unconstification to all previously aliased
                   arguments. *)
                let all_aliases = VarHashtbl.find_all aliases ret_var in
                List.iter (fun (aliased, _) ->
                    Stack.push (fun_var, aliased) to_unconst
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
              pointer variable which aliases an existing variable or
              argument. *)
           match (trm_resolve_pointer_and_aliased_variable ret aliases) with
           (* If it is the case, we need it to be unconstified. *)
           | Some (_, aliased) -> 
                    Printf.printf "return in %s is aliasing %s\n" (var_to_string fun_var) (var_to_string aliased); Stack.push (fun_var, aliased) to_unconst
           | None -> ()
         end;
       trm_iter (aux aliases fun_var) fun_body
    | _ -> trm_iter (aux aliases fun_var) fun_body
  in
  (* This is the main entry point of the function from where the auxiliary
     function shall be called. *)
  (* Deconstruct the target function definition term. *)
  let error = "Apac_basic.identify_mutables_on: expected target to a function \
               definition." in
  let (var, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = VarHashtbl.find const_records var in
  (* Create a hash table for aliases to arguments of the function. *)
  let aliases : const_aliases = VarHashtbl.create 10 in
  (* Try to find the parent class of the function. If any, get all the member
     variables of the class and *)
  let class_siblings = match (find_parent_typedef_record p) with
    | Some (td) -> typedef_get_members td
    | None -> [] in
  (* add them to the table of aliases as well as *)
  if List.length class_siblings > 0 then
    begin
      const_record.is_class_method <- true;
    (*  List.iteri (fun index (name, typ) ->
          Printf.printf "Class sibling for aliases : %s\n" (var_to_string name);
          VarHashtbl.add aliases name (var, typ_get_degree typ)
        ) class_siblings*)
    end
  else const_record.is_const <- false;
  (* the arguments of the function itself. This is necessary in order to be able
     to identify possible aliases to these variables within the body of the
     function during the analysis using the auxiliary function defined above. *)
  List.iteri (fun index (name, typ) ->
      Printf.printf "Arg name for aliases : %s\n" (var_to_string name);
      VarHashtbl.add aliases name (name, typ_get_degree typ)
    ) args;
  (* Actually compute the dependencies of the function definition at [path]
     and fill [to_unconst]. *)
  aux aliases var body

(* [identify_mutables]: expects the target [tg] to point at a function
   definition. It recurses over the body of the target function definition in
   order to figure out which function arguments and possible aliases to
   arguments should not be constified and adds them to the [to_unconst] stack
   (global variable, see Part II.1). *)
let identify_mutables (tg : target) : unit =
  Target.iter (fun t p -> identify_mutables_on p (get_trm_at_path p t)) tg
