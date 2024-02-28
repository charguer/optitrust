open Ast
open Typ
open Trm
open Mark
open Target

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
    ~exit_label:Apac_macros.goto_label res_var body in
  (* Add the '__exit' label at the end of the sequence. *)
  let body' = trm_seq_add_last
                (trm_add_label Apac_macros.goto_label (trm_unit())) body' in
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
    Target.apply_at_target_paths (use_goto_for_return_on mark) tg)

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
                 if const then (v, Apac_const.typ_constify ty) else (v, ty)
               ) tvs constify
  in
  (* Transform the multiple variable declaration into a sequence of simple
     variable declarations. *)
  let simple = List.map2 (fun tv ti -> trm_let vk tv ti) tvs' tis in
  (* Return a new sequence (without braces) containing the newly generated
     simple variable declarations. *)
    Syntax.trm_seq_no_brace simple

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

(* [unfold_function_calls tg]: expects target [tg] to point at a function
   definition. It moves all function calls under target [tg] out of variable
   declarations and nested function calls.

    Example:

          int a = f(g(2));

    becomes:

          int __var_1;
          __var_1 = g(2);
          int __var_2;
          __var_2 = f(__var_1);
          int a = __var_2;

    However:

          int a;
          a = f(g(2));

    becomes:

          int a;
          int __var_1;
          __var_1 = g(2);
          a = f(__var_1);

    as the call to 'f' is already dissociated from the declaration of 'a'. See
    also comments within the function.
*)
let unfold_function_calls (tg : target) : unit =
  Target.iter (fun t p ->
    (* Get the parent term to check whether it is an assignment (outside of a
       declaration). If it is the case, we do not need to apply the
       transformation. It would only create a superfluous variable. *)
    let parent_path = Path.parent p in
    let parent_target = target_of_path parent_path in
    if not (is_set_operation (get_trm_at_exn parent_target))
    then begin
      (* Define new intermediate variable. *)
      let var = fresh_var_name ~prefix:"__var_" () in
      (* Bind the return value of the current function call to that variable. *)
      Variable_basic.bind var (target_of_path p);
      (* Separate the assignment of the return value from the declaration of the
         variable. *)
      Variable_basic.init_detach [cVarDef var];
    end
  ) tg
