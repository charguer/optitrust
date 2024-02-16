open Syntax
open Target
open Path
open Apac_modules
open Apac_core

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

(* [task_group_on ?mark_group ~master t]: see [task_group] *)
let task_group_on ?(mark_group = false) ~(master : bool) (t : trm) : trm =
  (* Apply [Apac_core.task_group_mark] to the target sequence, if requested. *)
  let t' = if mark_group then trm_add_mark Apac_core.task_group_mark t else t in
  (* Draw the list of pragmas to apply. *)
  let pragmas = if master then
                [Parallel []; Master ; Taskgroup] else
                [Taskgroup] in
  (* Apply the pragmas on the target instruction sequence. *)
  trm_add_pragmas pragmas t'

(* [task_group ?mark_group ~master t]: puts the instruction sequence of a
   function's body into an OpenMP task group, i.e. a block of instructions
   delimited by curly brackets and prepended with the OpenMP pragma '#pragma omp
   taskgroup'.

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

    [mark_group] - decides whether the [Apac_core.task_group_mark] shall be
                   added to the resulting task group sequence;
    [master] - decides whether extra pragmas should be added for limiting the
               execution of the task group to only thread, the master thread;
    [t] - AST of a function body.
*)
let task_group ?(mark_group = false) ~(master : bool) (tg : target) : unit =
  Target.apply_at_target_paths (task_group_on ~mark_group ~master:master) tg

(* [use_goto_for_return_on mark t]: see [use_goto_for_return]. *)
let use_goto_for_return_on (mark : mark) (t : trm) : trm =
  (* Deconstruct the target function definition AST term. *)
  let error =
    "Apac_basic.use_goto_for_return_on: expected a target to a function \
     definition." in
  let (var, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Within the function's body, replace return statements with assignments to a
     return variable '__res' (if the return type is other than 'void') and
     gotos to an exiting label [Apac_core.goto_label]. The result is a sequence.
     Note that both the return variable and the exiting label are defined in the
     upcoming steps. *)
  let res_var = new_var "__res" in
  let body', _ = Internal.replace_return_with_assign ~check_terminal:false
    ~exit_label:Apac_core.goto_label res_var body in
  (* Add the '__exit' label at the end of the sequence. *)
  let body' = trm_seq_add_last (trm_add_label Apac_core.goto_label (trm_unit()))
                body' in
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
  trm_let_fun ~annot:t.annot var ret_ty args body'

(* [use_goto_for_return mark]: expects the target [tg] to point at a function
    definition. It replaces potentially multiple return statements by a single
    return statement at the end of the function definition through the usage of
    gotos.

    First of all, the transformation wraps the function's body into a sequence
    and marks it with [mark] if [mark] <> "". Then,

    if the function is of type 'void', it:
        1) replaces each return statement inside the new sequence with
           'goto __exit',
        2) appends an empty exiting label [Apac_core.goto_label] to the
           sequence;
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

(* [unfold_let_mult_on ?constify t]: see [unfold_let_mult]. *)
let unfold_let_mult_on ?(constify : bool list = []) (t : trm) : trm =
  (* Deconstruct the target multiple variable declaration term [t] into the
     variable kind [vk], the list of typed variables [tvs] being declared and
     the list of initialization terms [tis]. *)
  let error = "Apac_basic.unfold_let_mult_on: expected a target to a multiple \
               variable declaration." in
  let (vk, tvs, tis) = trm_inv ~error trm_let_mult_inv t in
  (* In addition to the unfolding of the target multiple variable declaration,
     this transformation allows for constifying one or more of the underlying
     declarations. Which declarations will be constified, if any, will be
     determined by the list of booleans [constify]. This list contains one
     boolean value for each element in [tvs] telling whether the corresponding
     declaration should be constified or not. This information is determined
     earlier during the constification process in [constify_args] and can be
     found in the hash table [Apac_core.const_mult]. 

     If [constify] is empty, no constification should take place. In this case,
     [constify] becomes a list full of [false] values. *)
  let constify =
    if constify <> [] then
      constify
    else
      List.init (List.length tvs) (Fun.const false)
  in
  (* Perform the constification of the concerned variable declarations. *)
  let tvs' = List.map2 (
                 fun (v, ty) const ->
                 if const then (v, typ_constify ty) else (v, ty)
               ) tvs constify
  in
  (* Transform the multiple variable declaration into a sequence of simple
     variable declarations. *)
  let simple = List.map2 (fun tv ti -> trm_let vk tv ti) tvs' tis in
  (* Return a new sequence (without braces) containing the newly generated
     simple variable declarations. *)
    trm_seq_no_brace simple

(* [unfold_let_mult ~constify tg]: expects target [tg] to point
   at a multiple variable declaration. Then, it replaces it by a sequence of
   simple variable declarations.

   For example:

     int a = 1, b = a;

   becomes:

     int a = 1;
     int b = a; 

   If [constify] is [true] one or more variable declarations from the target
   multiple variable declaration [tg] will be constified. See comments in
   [unfold_let_mult_on] for more details. *)
let unfold_let_mult ?(constify : bool list = []) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
      Target.apply_at_target_paths (unfold_let_mult_on ~constify) tg)

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
    let const_record = Var_Hashtbl.find Apac_core.const_records var in
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
             Hashtbl.add Apac_core.to_const_mult mark which_aliases;
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
  let const_record = Var_Hashtbl.find Apac_core.const_records var in
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
              if arg_cr.is_const then
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

(* [heapify_on t]: see [heapify]. *)
let heapify_on (t : trm) : trm =
  (* [heapify_on.single ?reference ?mult v ty init] is an auxiliary function for
     processing a single variable declaration represented by the variable [v] of
     type [ty] and by the initialization term [init]. It is thus possible to set
     the [mult] parameter to [true] and call it multiple times in a loop in the
     case of a multiple variable declaration. The [reference] parameter is
     useful in the case of a simple variable declaration when the reference
     status has to be determined before calling the function as the information
     cannot be restored from within the function like in the case of a multiple
     variable declaration. *)
  let single ?(reference = false) ?(mult = false)
        (v : var) (ty : typ) (init : trm) : typed_var * trm =
    (* Depending on whether it is a multiple [mult] is [true] or a single
       variable declaration [mult] is [false], the encoding of the type and the
       const status in the OptiTrust AST differs. *)
    if mult then
      begin
        (* Acquire the inner type of [ty], e.g. [const int] from [const
           int[5]]. *)
        let tyi = get_inner_type ty in
        (* If we are declaring a reference, *)
        if is_reference ty then
          begin
            (* we have to do something only when it is a constant reference to a
               literal value, i.e. it is not referencing a previously
               user-allocated memory location. *)
            if is_typ_const tyi && not (trm_can_resolve_pointer init) then
              (* In the case of a multiple variable declaration, [ty] is not a
                 reference yet. We have to both constify it and assign it the
                 reference type. *)
              ((v, typ_ptr Ptr_kind_ref (typ_const ty)), trm_new tyi init)
            (* Otherwise, we return the declarations elements as they are. *)
            else
              ((v, ty), init)
          end
        (* Otherwise, we distinguish three different cases: *)
        else
          begin
            (* 1) The value as well as the memory location are const, e.g. in
               [int const * a = &i, * const b = &j] it is the case of [b]. *)
            if is_typ_const ty then
              begin
                (* If the variable is already a pointer, we do not need to
                   transform it to a pointer type. *)
                let ty2 =
                  if is_typ_ptr (get_inner_const_type tyi) then
                    ty
                  else typ_ptr Ptr_kind_mut ty
                in
                (* If the variable is a pointer, we consider that it already
                   points to some data in the heap. If it is not the case, e.g.
                   in [int i = 1; int * a = &i], it is not of the responsibility
                   of this transformation function. Otherwise, we replace the
                   initial [init] an adequate allocation term.

                   Note that, here, we perform the test on the inner type
                   because [ty] is a const type in this case. *)
                let init2 = if is_typ_ptr tyi then init else trm_new ty init in
                (* Return the updated variable declaration and definition. *)
                ((v, ty2), init2)
              end
            (* 2) We are declaring a static array (of values, of pointers, const
               or not), e.g. [int * a[10]]. *)
            else if is_typ_array ty then
              begin
                (* To transform a static array allocation to a dynamic array
                   allocation, we need to determine its inner type, i.e. the
                   type of the values stored in the array (without the square
                   brackets), for the construction of the [new <inner-type>]
                   allocation term. Note that if the static variable is an array
                   of constants, e.g. [int const tab[2]], [tya] shall be [const
                   int]. *)
                let tya = get_inner_array_type tyi in
                (* We then transform the lvalue type to a pointer, e.g. [int
                   tab[2]] becomes [int * tab]. *)
                let ty2 = typ_ptr Ptr_kind_mut tya in
                (* If it is an array of constants, we also need to add [const]
                   to [const int * tab] so as it becomes [const int * const
                   tab]. *)
                let ty2 = if is_typ_const tya then typ_const ty2 else ty2 in
                (* Return the updated variable declaration and definition. *)
                ((v, ty2), trm_new ty init)
              end
            (* 3) Any other cases. Typically, we refer here to static variable
               declarations and definitions such as [int a = 1] or to variable
               declarations and definitions that were not fully constified, i.e.
               where either the value or the memory location is not const or
               none of the two. *)
            else
              begin
                (* If the variable is already a pointer, we do not need to
                   transform it to a pointer type. *)
                let ty2 =
                  if not (is_typ_ptr ty) then typ_ptr Ptr_kind_mut ty else ty in
                (* If the variable is a pointer, we consider that it already
                   points to some data in the heap. If it is not the case, e.g.
                   in [int i = 1; int * a = &i], it is not of the responsibility
                   of this transformation function.

                   Note that, here, we perform the test on directly [ty] is the
                   outer type is not a const type in this case. *)
                let init2 = if is_typ_ptr ty then init else trm_new ty init in
                (* Return the updated variable declaration and definition. *)
                ((v, ty2), init2)
              end
          end
      end
    else
      begin
        (* Acquire the inner type of [ty], e.g. [int] from [int *]. Note that in
           this example, [int *] actually encodes a statically allocated [int].
           This encoding is present only in the case of simple variable
           declarations. This is why we use [get_inner_ptr_type] here instead of
           [get_inner_type] like in the case of multiple variable
           declarations. *)
        let tyi = get_inner_ptr_type ty in
        (* If we are declaring a reference, *)
        if reference then
          (* we have to apply a get operation on the initialization term.
             Otherwise, OptiTrust shall add a [&] operator to the latter, e.g.
             [const int &b = 1] would become [const int &b = &1]. This behavior
             is present only in the case of a single variable declaration and is
             independent of the condition below. *)
          let init2 = trm_get init in
          begin
            (* Here, we have to do something only when it is a constant
               reference to a literal value, i.e. it is not referencing a
               previously user-allocated memory location. *)
            if is_typ_const tyi && not (trm_can_resolve_pointer init) then
              (* In the case of a simple variable declaration, [ty] is already a
                 reference. We only have to constify it. *)
              ((v, typ_const ty), trm_new tyi init2)
            else
              (* The above affirmation is true only in the case of constant
                 references. For mutable references, we have to restore the
                 reference type of [tyi]. Otherwise, a [int &e = i] results in
                 [int * e = i] even if the heapification is not performed. *)
              ((v, typ_ptr Ptr_kind_ref tyi), init2)
          end
        (* Otherwise, we distinguish three different cases: *)
        else
          begin
            (* 1) The value as well as the memory location are const, e.g. in
               [int const * a = &i, * const b = &j] it is the case of [b]. *)
            if is_typ_const ty then
              begin
                (* In the case of a simple variable declaration, we acquire the
                   inner const type directly from [ty] unlike in the case of a
                   multiple variable declaration (see above). *)
                let tyc = get_inner_const_type ty in
                (* If the variable is already a pointer, we do not need to
                   transform it to a pointer type. *)
                let ty2 =
                  if is_typ_ptr tyc then tyc else typ_ptr Ptr_kind_mut tyc in
                (* Then, we have to restore the const status of the variable. *)
                let ty2 = typ_const ty2 in
                (* If the variable is a pointer, we consider that it already
                   points to some data in the heap. If it is not the case, e.g.
                   in [int i = 1; int * a = &i], it is not of the responsibility
                   of this transformation function. Otherwise, we replace the
                   initial [init] an adequate allocation term.

                   Note that, here, we perform the test on the inner type
                   because [ty] is a const type in this case. *)
                let init2 = if is_typ_ptr tyc then init else trm_new ty init in
                (* Return the updated variable declaration and definition. *)
                ((v, ty2), init2)
              end
            (* 2) We are declaring a static array (of values, of pointers, const
               or not), e.g. [int * a[10]]. *)
            else if is_typ_array tyi then
              begin
                (* To transform a static array allocation to a dynamic array
                   allocation, we need to determine its inner type, i.e. the
                   type of the values stored in the array (without the square
                   brackets), for the construction of the [new <inner-type>]
                   allocation term. Note that if the static variable is an array
                   of constants, e.g. [int const tab[2]], [tya] shall be [const
                   int]. *)
                let tya = get_inner_array_type tyi in
                (* We then transform the lvalue type to a pointer, e.g. [int
                   tab[2]] becomes [int * tab]. *)
                let ty2 = typ_ptr Ptr_kind_mut tya in
                (* If it is an array of constants, we also need to add [const]
                   to [const int * tab] so as it becomes [const int * const
                   tab]. *)
                let ty2 = if is_typ_const tya then typ_const ty2 else ty2 in
                (* The transformation of [init] to an adequate allocation term
                   is required only in the case of an array of constants.
                   Otherwise, it is done implicitly by OptiTrust. *)
                let init2 =
                  if is_typ_const tya then trm_new ty init else init in
                (* Return the updated variable declaration and definition. *)
                ((v, ty2), init2)
              end
            (* 3) Any other cases. Typically, we refer here to static variable
               declarations and definitions such as [int a = 1] or to variable
               declarations and definitions that were not fully constified, i.e.
               where either the value or the memory location is not const or
               none of the two. *)
            else
              (* We then transform the lvalue type to a pointer, e.g. [int a]
                 becomes [int * a]. *)
              let ty2 = typ_ptr Ptr_kind_mut tyi in
              (* Then, we have to restore the const status of the variable if it
                 was present in the original inner type [tyi]. *)
              let ty2 = if is_typ_const tyi then typ_const ty2 else ty2 in
                (* The transformation of [init] to an adequate allocation term
                   is required only in the case of an array of constants.
                   Otherwise, it is done implicitly by OptiTrust. *)
              let init2 = if is_typ_const tyi then trm_new ty init else init in
              (* Return the updated variable declaration and definition. *)
              ((v, ty2), init2)
          end
      end
  in
  (* [t] must be either *)
  match t.desc with
  (* a simple variable declaration, or *)
  | Trm_let (kind, (v, ty), init, _) ->
     (* Call [heapify_on.single] once. *)
     let ((v2, ty2), init2) =
       single ~reference:(trm_has_cstyle Reference t) v ty init in
     (* Return an update single variable declaration term. *)
     trm_let kind (v2, ty2) init2
  (* a multiple variable declaration. *)
  | Trm_let_mult (kind, vtys, inits) ->
     (* Call [heapify_on.single] multiple times in a loop over all the
        declarations in the multiple variable declaration term. *)
     let updated = List.map2 (
                       fun (v, ty) init -> single ~mult:true v ty init
                     ) vtys inits in
     (* The upcoming instruction expects the updates typed variables and
        initialization terms in separate lists. *)
     let (vtys2, inits2) = List.split updated in
     (* Return an update multiple variable declaration term. *)
     trm_let_mult kind vtys2 inits2
  | _ -> fail t.loc "Apac_basic.heapify: expected a target to a variable \
                     declaration or a multiple variable declaration."

(* [heapify tg]: expects the target [tg] to point at a simple or a multiple
   variable declaration. Then, if it is necessary, i.e. if the variable is not a
   reference or a pointer to a previously user-allocated memory location, the
   function shall promote the variable from the stack to the heap.

   Example:

   int tab[5] = { 1, 2, 3, 4, 5 };

   becomes:

   int * tab = new int[5] { 1, 2, 3, 4, 5 }

   However, in:

   int * a = new int(10);
   int &b = &a;

   nor [a] nor [b] are transformed. *)
let heapify (tg : target) : unit =
  Target.apply_at_target_paths (heapify_on) tg

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
