open Ast
open Typ
open Trm
open Mark
open Path
open Target
open Tools
open Apac_miscellaneous
open Apac_const
open Apac_records

(* Create a stack of arguments that must not be constified. See [const_unconst]
   for more details. *)
let to_unconst : const_unconst = Stack.create ()
let to_unconst_objects : const_unconst_objects = Stack.create ()

(* [const_mult]: hash table for multiple variable declarations that must be
   transformed into sequences of simple variable declarations while constifying
   one or more of the variables being declared. See [const_mult]. *)
let to_const_mult : const_mult = Hashtbl.create 10

(* [unconstify_mutables] proceeds in two phases, i.e. [unconstify_to_unconst]
   and [unconstify_to_unconst_objects]. At first, it pops out the elements from
   [to_unconst] and then from [to_unconst_objects] (global variables, see Part
   II.1) one by one and propagates the unconstification through the concerned
   function constification records in [const_records] (global variable, see Part
   II.1). *)
let unconstify_mutables () : unit =
  (* [unconstify_to_unconst] propagates the unconstification through the
     concerned function constification records but it does not apply to
     arguments representing objects. Indeed, such an argument cannot be
     constified if it is used to call a non-const class method which may modify
     one or more members of the class. However, this information is not know
     before the end of the unconstification propagation, i.e. before returning
     from [unconstify_to_unconst]. This is why a second phase is necessary, see
     [unconstify_to_unconst_objects]. *)
  let rec unconstify_to_unconst () : unit =
    (* Pop out an element from [to_unconst]. *)
    match Stack.pop_opt to_unconst with
    | Some (fn, ar) ->
       (* Find the corresponding function constification record in
          [const_records]. *)
       let const_record = Var_Hashtbl.find const_records fn in
       (* If the argument position is set to -1, *)
       if ar = -1 then
         (* it means that the function is a class member method and a class
            sibling is being modified within its body. Therefore, the function
            must not be consitifed. *)
         begin
           if const_record.is_class_method then const_record.is_const <- false
         end
       else
         begin
           (* Otherwise, we try to gather the corresponding argument
              constification record. *)
           if (Int_map.mem ar const_record.const_args) then
             begin
               let arg = Int_map.find ar const_record.const_args in
               (* Then, if needed, *)
               if arg.is_const then
                 begin
                   (* we unconstify the argument *)
                   arg.is_const <- false;
                   (* and push to [to_unconst] all of the function arguments
                      that should be unconstified by propagation. In other
                      terms, we need to follow the dependencies too. *)
                   Var_map.iter (
                       fun k e -> Stack.push (k, e) to_unconst
                     ) arg.to_unconst_by_propagation;
                 end
             end
           else
             (* If it is not possible, fail. This is not normal! *)
             begin
               let error =
                 Printf.sprintf
                   "Apac_core.unconstify_mutables.unconstify_to_unconst: \
                    the constification record of '%s' has no argument \
                    constification record for the argument on position '%d'."
                   (var_to_string fn)
                   ar
               in
               fail None error
             end
         end;
       (* Recurse. *)
       unconstify_to_unconst ()
    (* When the stack is empty, stop and return. *)
    | None -> ()
  in
  (* [unconstify_to_unconst_objects] propagates the unconstification through the
     concerned function constification records and applies exclusively to
     arguments representing objects. If such an argument is used to call a
     non-const class method which may modify one or more members of the class,
     the argument is unconstified. *)
  let rec unconstify_to_unconst_objects () : unit =
    (* Pop out an element from [to_unconst_objects]. *)
    match Stack.pop_opt to_unconst_objects with
    | Some (fn, ar, ff) ->
       (* Find the constification record of the function that has been called,
          i.e. [ff] . *)
       let const_record_ff = Var_Hashtbl.find const_records ff in
       (* If it is a class member method and it has been unconstified in the
          previous phase, *)
       if const_record_ff.is_class_method && not const_record_ff.is_const then
         begin
           (* find the constification record of the function that has called
              [ff], i.e. [fn], and *)
           let const_record_fn = Var_Hashtbl.find const_records fn in
           (* try to gather the corresponding argument constification record. *)
           if (Int_map.mem ar const_record_fn.const_args) then
             begin
               let arg = Int_map.find ar const_record_fn.const_args in
               (* Then, if needed, *)
               if arg.is_const then
                 begin
                   (* we unconstify the argument *)
                   arg.is_const <- false;
                 end
             end
           else
             (* If it is not possible, fail. This is not normal! *)
             begin
               let error =
                 Printf.sprintf
                   "Apac_core.unconstify_mutables.\
                    unconstify_to_unconst_objects: the constification record \
                    of '%s' has no argument constification record for the \
                    argument on position '%d'."
                   (var_to_string fn)
                   ar
               in
               fail None error
             end
         end;
       (* Recurse. *)
       unconstify_to_unconst_objects ()
    (* When the stack is empty, stop and return. *)
    | None -> ()
  in
  unconstify_to_unconst ();
  unconstify_to_unconst_objects ()

(* [build_constification_records_on]: see [build_constification_records]. *)
let build_constification_records_on (t : trm) : unit =
  (* Deconstruct the function definition term. *)
  let error = "Apac_basic.const_lookup_candidates: expected a target to a \
               function definition!" in
  let (var, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
  (* If the function is a class member method, its first argument is the [this]
     variable referring to the parent class. In this case, we do not need to
     include it in the resulting constification record. *)
  let args =
    if var.name <> "main" && (List.length args) > 0 then
      (* Extract the first argument of the function. *)
      let (first, _) = List.hd args in
      if first.name = "this" then List.tl args else args
    else args in
  (* Create a hash table for locally defined variables. *)
  let locals : symbols = Var_Hashtbl.create 10 in
  (* Create an argument constification record for the function's arguments and
     fill [locals] with function's arguments and their pointer degrees. *)
  let const_args = List.mapi (
                       fun pos (arg, ty) ->
                       let deg = typ_get_degree ty in
                       let _ = Var_Hashtbl.add locals arg deg in
                       (
                         pos,
                         {
                           is_ptr_or_ref =
                             is_typ_ptr ty || is_typ_ref ty ||
                               is_typ_array ty;
                           is_const = true;
                           to_unconst_by_propagation = Var_map.empty;
                         }
                       )
                     ) args in
  let const_args = Int_map.of_seq (List.to_seq const_args) in
  (* Create the constification record for the function itself *)
  let const : const_fun = {
      const_args = const_args;
      is_const = true;
      is_ret_ptr = is_typ_ptr ret_ty;
      is_ret_ref = is_typ_array ret_ty || is_typ_ref ret_ty;
      is_class_method = false;
      task_graph = None;
      variables = locals;
      ast_backup = t;
    } in
  (* and add it to [const_records] (global variable, see Part II.1) if it is not
     present in the hash table already, e.g. in the case of a
     pre-declaration. *)
  if not (Var_Hashtbl.mem const_records var) then
    begin
      Var_Hashtbl.add const_records var const
    end
      
(* [build_constification_records]: expects the target [tg] to point at a
   function definition. It adds a new entry into [const_records] (global
   variable, see Part II.1) based on the information about the function. *)
let build_constification_records (tg : target) : unit =
  Target.iter_at_target_paths (build_constification_records_on) tg

(* [identify_mutables_on p t]: see [identify_mutables]. *)
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
    | Trm_apps ({ desc = Trm_var (_ , name); _ }, args) ->
       (* If we known the function's definition, i.e. the function was defined
          within the scope of the analysis, *)
       if Var_Hashtbl.mem const_records name then
         begin
           (* find the corresponding function constification record containing
              the constification records of all of the arguments. *)
           let fun_call_const = Var_Hashtbl.find const_records name in
           let fun_args_const = fun_call_const.const_args in
           (* In the case of a call to a class member method, the first argument
              is the variable referring to the parent class instance, e.g. in
              [this->p(i, j)] the first argument is [this] and in [a.f(i, j)]
              the first argument is [a]]. This is why the number of arguments in
              the function and the number of argument constification records
              associated with the function may not be the same. If [shift], the
              difference of these two values, has a positive value, we know that
              the current function call is a call to a class member method. *)
           let shift = (List.length args) - (Int_map.cardinal fun_args_const) in
           (* For each argument of the function call, we *)
           List.iteri (fun index arg ->
               (* go through the access and reference operations to obtain the
                  argument in the form of a labelled variable, if any, and *)
               match (trm_strip_accesses_and_references_and_get_lvar arg) with
               | Some arg_lvar ->
                  (* if the variable is an alias, we have to *)
                  if LVar_Hashtbl.mem aliases arg_lvar then
                    begin
                      (* determine the position of the argument it is
                         aliasing. *)
                      let (aliased, _) = LVar_Hashtbl.find aliases arg_lvar in
                      (* If a class member method has been called and if the
                         parent is not [this], we may have to unconstify the
                         argument. See [unconstify_mutables] for more details.
                         The constification transformation does not take into
                         account class member variables. *)
                      if (index - shift) < 0 && arg_lvar.v.name <> "this" then
                        Stack.push
                          (fun_var, aliased, name) to_unconst_objects
                      else
                        (* In the opposite case, *)
                        begin
                          (* there is the corresponding argument constification
                             record to be gathered *)
                          let pos = index - shift in
                          if (Int_map.mem pos fun_args_const) then
                            begin
                              let arg_const = Int_map.find pos fun_args_const in
                              (* and if the latter is a pointer or a
                                 reference, *)
                              if arg_const.is_ptr_or_ref then
                                begin
                                  (* we may have to unconstify it by
                                     propagation. *)
                                  arg_const.to_unconst_by_propagation <-
                                    Var_map.add fun_var aliased
                                      arg_const.to_unconst_by_propagation
                                end
                            end
                          else
                            (* If it is not possible, fail. This is not
                               normal! *)
                            begin
                              let error =
                                Printf.sprintf
                                  "Apac_core.identify_mutables_on.aux: \
                                   the constification record of '%s' has no \
                                   argument constification record for the \
                                   argument on position '%d'."
                                  (var_to_string fun_var)
                                  pos
                              in
                              fail None error
                            end
                        end
                    end
               | None -> ()
             ) args;
         end
       (* Otherwise, we do not have other choice but consider that the
          function call may modify its arguments by side-effect. Therefore,
          if the function call involves one or more arguments or alises to
          arguments of the currently processed function definition
          [fun_var], we have to unconstify them. *)
       else
         begin
           (* Let us warn the user about that. *)
           Printf.printf
             "WARNING: missing definition of '%s', considering all arguments \
              of the call as in-out dependencies\n" (var_to_string name);
           (* Then, for each argument of the function call, we *)
           List.iteri (fun index arg ->
               (* go through the access and reference operations to obtain the
                  argument in the form of a labelled variable, if any. *)
               match (trm_strip_accesses_and_references_and_get_lvar arg) with
               | Some arg_lvar ->
                  (* If the variable is an alias, we have to *)
                  if LVar_Hashtbl.mem aliases arg_lvar then
                    begin
                      (* determine the position of the argument it is aliasing
                         and *)
                      let (aliased, _) = LVar_Hashtbl.find aliases arg_lvar in
                      (* unconstify it. *)
                      Stack.push (fun_var, aliased) to_unconst
                    end
               | None -> ()
             ) args;
         end;
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
    | Trm_apps (_, [lval; rval]) when is_set_operation fun_body ->
       begin
         (* The lvalue has been modified by assignment. Resolve the labelled
            variable behind the lvalue and determine whether it has been
            dereferenced. *)
         match trm_resolve_binop_lval_and_get_with_deref ~plus:true lval with
         | Some (lval_lvar, lval_deref) ->
            (* If the lvalue is [this], it means that the function we are in is
               modifying a member variable of the parent class. Therefore, we
               will have to unconstify the method itself. *)
            if lval_lvar.v.name = "this" then
              begin
                Stack.push (fun_var, -1) to_unconst
              end;
            (* If it is an argument or an alias to an argument which was
               dereferenced or a reference, we must not constify it. *)
            if LVar_Hashtbl.mem aliases lval_lvar then
              let (lval_aliased, lval_degree) =
                LVar_Hashtbl.find aliases lval_lvar in
              let lval_aliased = if lval_lvar.v.name = "this" then
                                   lval_aliased + 1
                                 else lval_aliased in
              let fun_call_const = Var_Hashtbl.find const_records fun_var in
              let fun_args_const = fun_call_const.const_args in
              let arg_const = Int_map.find lval_aliased fun_args_const in 
              if lval_deref || (lval_degree < 0 && arg_const.is_ptr_or_ref) then
                (* An alias of the same name may have been used multiple times
                   to alias different memory locations.
                   
                   For example, in:
                   
                   L1: void f(int * a, int * b, int * c) {
                   L2:   int * d = a;
                   L3:   d = c; 
                   L4:   *d = 1;
                   L5: }
                   
                   [d] is declared as an alias to [a] on L2. The data pointed to
                   by [a] is not modified within the function. On L3, [d]
                   becomes an alias for [c]. Then, on L4, the data pointed to by
                   [c] is modified through its alias [d]. Therefore, the
                   analysis will conclude that nor [c] nor [d] should be
                   constified. However, for [a] it will conclude that the
                   argument can be constified as the data it is pointing to is
                   never modified within the function. In the end, this will
                   produce a compilation error as [a], which became const, is
                   assigned to the non-const [d] on L2.
                   
                   In order to prevent this situation from happening, we must
                   propagate the unconstification to previously aliased
                   arguments too. As the [aliases] hash table stores all the
                   values that were ever assigned to a given key, we only have
                   to [find_all] of them and push them to the unconstification
                   stack. *)
                let all_aliases = LVar_Hashtbl.find_all aliases lval_lvar in
                List.iter (fun (aliased, _) ->
                    (* Again, we do not consider parent class members because
                       the constification process does not analyze entire
                       classes. *)
                    if aliased > -1 then
                      Stack.push (fun_var, aliased) to_unconst
                  ) all_aliases
              else
                (* When an alias changes a target, i.e. when the lvalue variable
                   was not dereferenced, *)
                begin
                  (* we have to resolve the pointer labelled variable behind the
                     rvalue and check if it is an argument an alias to an
                     argument. *)
                  match (
                    trm_resolve_pointer_and_aliased_variable rval aliases
                  ) with
                  (* If it is the case, we have to add a new entry into
                     [aliases]. This happens, for example, on L3 in the
                     aforementioned example. *)
                  | Some (_, aliased) ->
                     (* The value of [lval_deref] is incorrect when the lvalue
                        refers to the parent [this]. This is because
                        [trm_resolve_binop_lval_and_get_with_deref] operates on
                        [this] and not on the concerned structure member. In
                        this case, to verify that the lvalue was not
                        dereferenced, we have to check that the pointer degree
                        of the lvalue is greater than 0. *)
                     if
                       not lval_deref ||
                         (lval_lvar.v.name = "this" && lval_degree <> 0) then
                       LVar_Hashtbl.add aliases
                         lval_lvar (aliased, lval_degree)
                  | None -> ()
                end
         | _ -> ()
       end;
       trm_iter (aux aliases fun_var) fun_body
    (* Increment or decrement unary operation: update the unconstification
       stack. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [t]) when
           (is_prefix_unary op) || (is_postfix_unary op) ->
       begin
         match trm_resolve_binop_lval_and_get_with_deref t with
         | Some (lval_lvar, lval_deref) ->
            if LVar_Hashtbl.mem aliases lval_lvar then
              let (lval_aliased, lval_degree) =
                LVar_Hashtbl.find aliases lval_lvar in
              let lval_aliased = if lval_lvar.v.name = "this" then
                                   lval_aliased + 1
                                 else lval_aliased in
              let fun_call_const = Var_Hashtbl.find const_records fun_var in
              let fun_args_const = fun_call_const.const_args in
              let arg_const = Int_map.find lval_aliased fun_args_const in 
              if lval_deref || (lval_degree < 0 && arg_const.is_ptr_or_ref) then
                (* Propagate the unconstification to all previously aliased
                   arguments. *)
                let all_aliases = LVar_Hashtbl.find_all aliases lval_lvar in
                List.iter (fun (aliased , _) ->
                    (** We do not consider parent class members because the
                        constification process does not analyze entire classes
                        yet. *)
                    if aliased > -1 then
                      Stack.push (fun_var, aliased) to_unconst
                  ) all_aliases
         | None -> fail fun_body.loc "Apac_constify.identify_mutables_on.aux: \
                                      unable to retrieve variable name"
       end;
       trm_iter (aux aliases fun_var) fun_body
    (* Return statement: update the unconstification stack if the return value
       is a reference or a pointer. *)
    | Trm_abort (Ret (Some ret)) ->
       let fun_const = Var_Hashtbl.find const_records fun_var in
       if fun_const.is_ret_ref then
         begin
           (* If the return type of the function is a reference, there are two
              return term types we can possibly deal with: *)
           match (trm_resolve_var_in_unop_or_array_access_and_get ret) with
           (* 1) a variable term corresponding to the reference, *)
           | Some ret_lvar ->
              begin
                (* Propagate the unconstification to all previously aliased
                   arguments. *)
                let all_aliases = LVar_Hashtbl.find_all aliases ret_lvar in
                List.iter (fun (aliased, _) ->
                    (* Again, we do not consider parent class members because
                       the constification process does not analyze entire
                       classes yet. See an aforementioned TODO. *)
                    if aliased > -1 then
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
              (* Again, we do not consider parent class members because the
                 constification process does not analyze entire classes yet. See
                 an aforementioned TODO. *)
              if aliased > -1 then
                Stack.push (fun_var, aliased) to_unconst
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
  let _ = Printf.printf "Processing function %s\n" (var_to_string var) in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records var in
  (* Create a hash table for aliases to arguments of the function. *)
  let aliases : const_aliases = LVar_Hashtbl.create 10 in
  (* Try to find the parent class of the function. If any, get all the member
     variables of the class and *)
  let class_siblings = match (find_parent_typedef_record p) with
    | Some (td) -> typedef_get_members td
    | None -> [] in
  (* add them to the table of aliases in the form of labelled variables. *)
  if List.length class_siblings > 0 then
    begin
      (* In the case of class member methods, the first argument of the function
         is the variable referring to the parent class instance, i.e. [this]. *)
      let (this, _) = List.hd args in
      (* Record that this is a class member method. *)
      const_record.is_class_method <- true;
      (* For each member variable of the parent class, *)
      List.iteri (fun idx (label, ty) ->
          (* build the corresponding labelled variable and *)
          let lv : lvar = { v = this; l = label } in
          (* add it to the hash table of aliases. *)
          LVar_Hashtbl.add aliases lv ((- idx), typ_get_degree ty)
        ) class_siblings
    end;
  (* If the function is not a class member method, it cannot be constified. It
     is not taken into account in C/C++. *)
  if not const_record.is_class_method then
    const_record.is_const <- false;
  (* Then, of course, we have also to add the arguments of the function itself
     to the hash table of aliases. This is necessary in order to be able to
     identify possible aliases to these variables within the body of the
     function during the analysis using the auxiliary function defined above.

     Note that in the case of class member methods, we do not need to add the
     first argument which is a variable referring to the parent class instance.
     Indeed, we have already added sibling class member values into the hash
     table of aliases above. *)
  let args = if const_record.is_class_method then List.tl args else args in
  List.iteri (fun pos (v, ty) ->
      let lv : lvar = { v = v; l = String.empty } in
      let d = if (is_reference ty) then (-1) else typ_get_degree ty in
      LVar_Hashtbl.add aliases lv (pos, d)
    ) args;
  (* Actually compute the dependencies of the function definition at [path]
     and fill [to_unconst]. *)
  aux aliases var body

(* [identify_mutables tg]: expects the target [tg] to point at a function
   definition. It recurses over the body of the target function definition in
   order to figure out which function arguments and possible aliases to
   arguments should not be constified and adds them to the [to_unconst] stack
   (global variable, see Part II.1). *)
let identify_mutables (tg : target) : unit =
  Target.iter (fun t p -> identify_mutables_on p (get_trm_at_path p t)) tg

(* [constify_args_on ?force t]: see [constify_args]. *)
let constify_args_on ?(force = false) (t : trm) : trm =
  (* Try to deconstruct the target function definition term. *)
  let error = "Apac_basic.constify_args_on: expected a target to a function \
               definition." in
  let (var, ret_typ, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Optionally, force the constification of all of the function's arguments as
     well as the constification of the function itself. *)
  if force then
    begin
      (* Constify all of the function's arguments. *)
      let const_args = List.map (fun (v, ty) -> (v, (typ_constify ty))) args in
      (* Rebuild the function definition term using the list of constified
         arguments and constify it. *)
      trm_add_cstyle Const_method (
          trm_let_fun ~annot:t.annot var ret_typ const_args body
        )
    end
  (* Otherwise, have a look at the constification record of the function to find
     out which of its arguments should be constified, if any, and whether the
     function itself should be constified. *)
  else
    (* Gather the constification record of the function. *)
    let const_record = Var_Hashtbl.find const_records var in
  (* If the function is a class member method, its first argument is the [this]
     variable referring to the parent class. In this case, we do not need to
     include it in the resulting constification record. *)
    let args' = if const_record.is_class_method then List.tl args else args in
    (* Simultaneously loop over the list of function's arguments as well as over
       the list of argument constification records and constify the arguments
       that should be constified according to the corresponding constification
       record. *)
    let const_args = List.mapi (fun pos (v, ty) ->
                         if (Int_map.mem pos const_record.const_args) then
                           begin
                             let cr =
                               Int_map.find pos const_record.const_args in
                             if cr.is_const then
                               (v, (typ_constify ty))
                             else
                               (v, ty)
                           end
                         else
                           (* If the argument constification record does not
                              exist, fail. This is not normal! *)
                           begin
                             let error =
                               Printf.sprintf
                                 "Apac_basic.constify_args_on: the \
                                  constification record of '%s' has no \
                                  argument constification record for the \
                                  argument on position '%d'."
                                 (var_to_string var)
                                 pos
                             in
                             fail None error
                           end
                       ) args' in
    (* Rebuild the function definition term using the list of constified
       arguments. We have to bring back [this] to the list of arguments. If it
       is a class member method, [this] is a non-empty list. *)
    let const_args =
      if const_record.is_class_method then
        (List.hd args) :: const_args
      else
        const_args in
    let let_fun_with_const_args =
      trm_let_fun ~annot:t.annot var ret_typ const_args body in
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
      | Trm_while _ -> trm_map (aux aliases) t
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
    | Trm_let_mult (vk, tvs, tis) ->
       (* Check whether the declared variables represent aliases to function
          arguments or previously declared variables and return the test results
          in the form of a list of boolean values. *)
       let which_aliases : bool list =
         List.map2 (
             fun tv ti ->
             let res = trm_let_update_aliases tv ti aliases in res > 0
           ) tvs tis in
       (* If all the elements of [which_aliases] are [false], *)
       if (List.for_all (fun a -> a = false) which_aliases) then
         (* there are no aliases to constant variables and there is nothing
            todo. Return [t] unchanged. *)
         t
           (* If all the elements of [which_aliases] are [true], *)
       else if (List.for_all (fun a -> a = true) which_aliases) then
         (* we can safely constify all the variable declarations in the multiple
            variable declaration without splitting it into a sequence of simple
            variable declarations beforehand. *)
         let tvs' = List.map (fun (v, ty) -> (v, typ_constify ty)) tvs in
         trm_let_mult vk tvs' tis
       else
         begin
           (* If only some of the elements of [which_aliases] are [true] but the
              inner type of the multiple variable declaration is already
              constant, *)
           let (_, fty) = List.nth tvs 0 in
           if is_typ_const (get_inner_type fty) then
             (* we can complete the constification of selected variables within
                the multiple variable declaration without splitting it. *)
             let tvs' = List.map2 (
                            fun (v, ty) const ->
                            if const then (v, typ_constify ty) else (v, ty)
                          ) tvs which_aliases in
             trm_let_mult vk tvs' tis
           else
             (* Otherwise, we will have to split the multiple variable
                delcaration into a sequence of simple variable declarations and
                constify selected variables according to the values in
                [which_aliases]. However, this cannot be done directly within
                this transformation. Therefore, we mark the multiple variable
                declaration and store the necessary information in
                [Apac_core.to_const_mult] which will be processed in
                [Apac.constify]. See also [Apac_core.const_mult] and
                [unfold_let_mult]. *)
             let mark = "apac_let_mult" ^ (Mark.next ()) in
             Hashtbl.add to_const_mult mark which_aliases;
             (* At this stage, the original is left unchanged. We only mark
                it. *)
             trm_add_mark mark t
         end
    (* Other cases: do nothing and recurse deeper. *)
    | _ -> trm_map (aux aliases) t
  in
  (* This is the main entry point where the auxiliary function is called from.

     Deconstruct the function definition term. *)
  let error = "Apac_basic.constify_aliases_on: expected a target to a function \
               definition." in
  let (var, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Gather the constification record of the function. *)
  let const_record = Var_Hashtbl.find const_records var in
  (* If the function is a class member method, its first argument is the [this]
     variable referring to the parent class. In this case, we do not need to
     include it in the resulting constification record. *)
  let args' = if const_record.is_class_method then List.tl args else args in
  (* Create an alias hash table. *)
  let aliases : const_aliases = LVar_Hashtbl.create 10 in
  (* Optionally, force the constification of all of the aliases by adding all of
     the function arguments into the hash table of aliases. *)
  if force then
    begin
      List.iteri (fun arg_pos (arg_var, arg_ty) ->
          let arg_lv : lvar = { v = arg_var; l = String.empty } in
          LVar_Hashtbl.add aliases arg_lv (arg_pos, typ_get_degree arg_ty)
        ) args'
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
      List.iteri (
          fun arg_pos (arg_var, arg_ty) ->
          if (Int_map.mem arg_pos const_record.const_args) then
            begin
              let arg_cr = Int_map.find arg_pos const_record.const_args in
              if arg_cr.is_const && arg_cr.is_ptr_or_ref then
                begin
                  let arg_lv : lvar = { v = arg_var; l = String.empty } in
                  LVar_Hashtbl.add
                    aliases arg_lv (arg_pos, typ_get_degree arg_ty)
                end
            end
          else
            (* If the argument constification record does not exist, fail. This
               is not normal! *)
            begin
              let error =
                Printf.sprintf
                  "Apac_basic.constify_aliases_on: the constification record \
                   of '%s' has no argument constification record for the \
                   argument on position '%d'."
                  (var_to_string var)
                  arg_pos
              in
              fail None error
            end
        ) args'
    end;
  (* Call the auxiliary function to constify the aliases in the body of the
     function. *)
  let body_with_const_aliases = aux aliases body in
  (* Rebuild and return the function definition term with the updated body. *)
  trm_let_fun ~annot:t.annot var ret_ty args body_with_const_aliases

(* [constify_aliases ?force tg]: expects target the target [tg] to point at a
   function definition. Then, based on the constification records in
   [const_records], it shall constify the aliases to arguments that have been
   constified during the constification process.

   One can force, e.g. for testing purposes, the function to ignore the
   constification records and to constify the aliases to all of the function's
   arguments. *)
let constify_aliases ?(force = false) (tg : target) : unit =
  Target.apply_at_target_paths (constify_aliases_on ~force) tg

(* [constify tg]: expects target [tg] to point at a function definition. It
   constifies function arguments and functions whenever it is possible depending
   on data accesses, aliases and dependencies. *)
let constify (tg : target) : unit =
  (* Step 1: Clear the hash table of constification records. *)
  Var_Hashtbl.clear const_records;
  (* Step 2: Iterate over [tg] and fill the hash table of constification
     records. *)
  build_constification_records tg;
  (* Step 3: Clear the stack of arguments and functions to unconstify after the
     analysis of data accesses, aliases and dependencies. Indeed, the
     constification algorithm begins by consedring that all functions and
     function arguments can be constified. It then performs an analysis (in
     Step 4) of data accesses, aliases and dependencies and unconstifies all of
     the variables (aliases), arguments and functions where the constification
     is not possible. *)
  Stack.clear to_unconst;
  (* Step 4: Perform an analysis of data accesses, aliases and dependencies,
     then decide which variables (aliases), arguments and functions should not
     be constified. *)
  identify_mutables tg;
  (* Step 5: Propagate the unconstification. *)
  unconstify_mutables ();
  (* Step 6: Effectively transform the AST so as to add 'const' keywords to
     function arguments, functions and *)
  constify_args tg;
  (* Step 7: aliases of function arguments. Here, we begin by clearing the
     global hash table of multiple variable declarations that are subject to
     partial constification, i.e. one or more of the underlying variable
     declarations should be constified. *)
  Hashtbl.clear to_const_mult;
  (* We then perform the constification of aliases. However, this transformation
     cannot perform partial constification. In such cases, it only collects the
     necessary information about the concerned multiple variable declarations
     and inserts it into [Apac_core.to_const_mult]. *)
  constify_aliases tg;
  (* At the end, we iterate over the key-value pairs of
     [Apac_core.to_const_mult], unfold the concerned multiple variable
     declarations having a specific mark (see [Apac_core.const_mult] and the
     comments in [Apac_basic.constify_arguments]) into sequences of simple
     variable declarations and constify those declaration which should be
     constified. *)
  Hashtbl.iter (fun k v ->
      Apac_prologue.unfold_let_mult ~constify:v (tg @ [cMark k])
    ) to_const_mult
