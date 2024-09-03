open Ast
open Typ
open Trm
open Mark
open Target
open Apac_records

(** [build_records_on]: see [!build_records]. *)
let build_records_on (t : trm) : unit =
  (** Explode the function definition term into the function name variable [f],
      its return type [ret_ty] and the list of its arguments [args]. *)
  let error = "Apac_prologue.build_records: expected a target to a function \
               definition!" in
  let (fn, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
  (** If [fn] is a class member method, its first argument in [args] is the
      [this] variable referring to the parent class. Ignore it. *)
  let args =
    (** This is necessary only if [fn] does not refer to the [main] function and
        if it has at least one argument. *)
    if fn.name <> "main" && (List.length args) > 0 then
      (** In this case, extract the first argument of the function and if it is
          [this], discard it. *)
      let (first, _) = List.hd args in
      if first.name = "this" then List.tl args else args
    else args in
  (** Build the function record of [fn] and *)
  let r = FunctionRecord.create args t in
  (** add it to [!functions] if it is not present in the hash table already,
      e.g. in the case of a pre-declaration. *)
  if not (Var_Hashtbl.mem functions fn) then
    Var_Hashtbl.add functions fn r
      
(** [build_records tg]: expects the target [tg] to point at a function
    definition and builds a function record for it (see [!type:f] and
    [!build_records_on]). *)
let build_records (tg : target) : unit =
  Target.iter_at_target_paths (build_records_on) tg

(** [select_candidates tg]: expects the target [tg] to point at a function
    definition. The pass then proceeds in two phases.

    First, it iterates over the target function definitions and marks the
    functions meeting the requirements of a taskification candidate with
    [!Apac_macros.candidate_mark]. To consider a function a taskification
    candidate, it must feature at least two calls to a function having a record
    in [!Apac_records.functions] or one call to such a function and one loop or
    two loop nests.

    Second, it iterates again over the target function definitions and marks
    with [!Apac_macros.candidate_mark] the functions calling any function
    carrying the [!Apac_macros.candidate_mark] mark as a result of the first
    phase. *)
let select_candidates (tg : target) : unit =
  (** Initialize a set of variables [candidates] to store the variables refering
      to functions meeting the taskification candidate requirements we mark with
      [!Apac_macros.candidate_mark] during the first phase of the pass so as to
      make this information available to the second phase of the pass. *)
  let candidates = ref Var_set.empty in
  (** Iterate over the target function definitions and store each function
      meeting the taskification candidate requirements in [candidates] and mark
      it with [!Apac_macros.candidate_mark]. *)
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the definition term [t] of the function [f]. *)
      let error = "Apac_prologue.select_candidates: expected a target to a \
                   function definition!" in
      let (f, _, _, body) = trm_inv ~error trm_let_fun_inv t in
      (** Count [calls] to functions having a record in
          [!Apac_records.functions] and [loops] in the [body] of [f]. *)
      let (calls, loops) =
        trm_fold (fun (c, l) t ->
            match t.desc with
            (** [t] is a call to a function in [!Apac_records.functions]. *)
            | Trm_apps ({ desc = Trm_var (_, f)}, _) when
                   Var_Hashtbl.mem functions f -> (c + 1, l)
            (** [t] is a loop. *)
            | Trm_for _
              | Trm_for_c _
              | Trm_while _ 
              | Trm_do_while _ -> (c, l + 1) 
            (** Otherwise, there is nothing to count. *)
            | _ -> (c, l)
          ) (0, 0) body in
      (** Check whether it contains at least two calls to function having a
          record in [!Apac_records.functions] or one call to such a function and
          at least one loop. *)
      let candidate =
        if (calls > 1) || (calls > 0 && loops > 0) then
          (** If this condition verifies, we can consider [f] a taskification
              candidate. *)
          true
        else if (loops > 1) then
          (** If it is not the case, but if there are at least two loops in the
              [body] of [f], check the latter for the presence of at least two
              loop nests. If this condition verifies, we can consider [f] a
              taskification candidate. *)
          trm_fold (fun n t ->
              match t.desc with
              (** [t] is a sequence of statements [s]. Check whether the latter
                  features at least two loop nests. *)
              | Trm_seq s ->
                 let l = Mlist.fold_left (fun acc e ->
                             match e.desc with
                             | Trm_for _
                               | Trm_for_c _
                               | Trm_while _
                               | Trm_do_while _ -> acc + 1
                             | _ -> acc
                           ) 0 s in
                 if l > 1 then n || true else n
              (** In any other case, there is nothing to check. *)
              | _ -> n
            ) false body
        else
          (** Otherwise, [f] does not meet the requirements of a taskification
              candidate. *)
          false
      in
      (** If [f] meets the requirements of a taskification candidate, add it to
          [candidates] and mark the target function definition term with
          [!Apac_macros.candidate_mark]. *)
      if candidate then
        begin
          candidates := Var_set.add f !candidates;
          trm_add_mark Apac_macros.candidate_mark t
        end
      else
        (** Otherwise, return the function definition term as-is. *)
        t
    ) tg;
  (** Iterate over the target function definitions, update [candidates] with
      functions calling a function in [candidates], i.e. with functions calling
      a taskficiation candidate, mark them with [!Apac_macros.candidate_mark]
      and continue until [candidates] stabilizes, i.e. does not receive any new
      elements.

      To this end, begin by initializing a variable [now] to keep the current
      number of elements in [candidates]. *)
  let now = ref (Var_set.cardinal !candidates) in
  (** Then, iterate over the target function definitions... *)
  let rec stabilize = fun () ->
    (** For each function definition, *)
    Target.apply_at_target_paths (fun t ->
        (** deconstruct the definition term [t] of the function [f], *)
        let error = "Apac_prologue.select_candidates: expected a target to a \
                     function definition!" in
        let (f, _, _, body) = trm_inv ~error trm_let_fun_inv t in
        (** loop over the [body] of [f] and *)
        let candidate =
          trm_fold (fun acc c ->
              match c.desc with
              (** if [f] has a call to a function [f'] in [candidates], *)
              | Trm_apps ({ desc = Trm_var (_, f')}, _)
                   when Var_set.mem f' !candidates -> acc || true 
              | _ -> acc
            ) false body in
        if candidate && not (trm_has_mark Apac_macros.candidate_mark t) then
          begin
            (** add [f] to [candidates] and *)
            candidates := Var_set.add f !candidates;
            (** mark it with [!Apac_macros.candidate_mark]. *)
            trm_add_mark Apac_macros.candidate_mark t
          end
        else
          t
      ) tg;
    (** ...while the number of elements in [candidates] increases. *)
    if (Var_set.cardinal !candidates) > !now then
      begin
        now := Var_set.cardinal !candidates;
        stabilize ()
      end
  in
  stabilize ()

(** [unify_returns tg]: expects the target [tg] to point at a function
    definition. It replaces [return] statements within the body of the latter by
    a single [return] statement through the usage of [goto] jumps.

    First of all, the transformation wraps the body of the target function into
    a sequence and marks it with [!Apac_macros.candidate_body_mark], as well as
    [!Apac_macros.candidate_body_mark] in the case of the [main] function. Then,
    if the function returns [void], it appends the [!Apac_macros.goto_label] to
    the sequence and replaces each [return] statement inside the sequence with a
    [goto] jumping to that label.

    For example, the following function definition

    {[
    void returns_nothing(int a, int b) {
      if(a > b) {
        // ... some work with a ...
        return;
      }
      // ... some work with b ...
    }
    ]}

    becomes

    {[
    void returns_nothing(int a, int b) {
      {
        if(a > b) {
          // ... some work with a ...
          goto __apac_exit;
        }
        // ... some work with b ...
    __apac_exit: ;
      }
    }
    ]}

    If the function returns a value, the transformation preprends the sequence
    with the declaration of [!Apac_macros.result_variable], appends the
    [!Apac_macros.goto_label] to the sequence and replaces each [return]
    statement inside the sequence with an assignment of the value to return to
    the [!Apac_macros.result_variable] and a [goto] jumping to
    [!Apac_macros.goto_label].

    For example, the following function definition

    {[
    void returns_something(int a, int b) {
      if(a > b) {
        // ... some work with a ...
        return a;
      }
      // ... some work with b ...
      return b;
    }
    ]}

    becomes

    {[
    void returns_something(int a, int b) {
      {
        if(a > b) {
          // ... some work with a ...
          __apac_ret = a;
          goto __apac_exit;
        }
        // ... some work with b ...
        __apac_ret = b;
        goto __apac_exit;
    __apac_exit: ;
      }
    }
    ]} *)
let unify_returns (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
      Target.apply_at_target_paths (fun t ->
          (** Deconstruct the definition term [t] of the function [f]. *)
          let error = "Apac_prologue.use_goto_for_return: expected a target to \
                       a function definition." in
          let (f, ty, args, body) = trm_inv ~error trm_let_fun_inv t in
          (** Unify the [return] statements within the [body] of [f]. The result
              is a new body sequence. *)
          let ret = new_var Apac_macros.result_variable in
          let (body, _) =
            Internal.replace_return_with_assign
              ~check_terminal:false
              ~exit_label:Apac_macros.goto_label ret body in
          (** Add [ret] into the function record, if it exists. This may not be
              the case when testing the transformation separately. *)
          if (Var_Hashtbl.mem Apac_records.functions f) then
            begin
              let r = Var_Hashtbl.find Apac_records.functions f in
              Var_Hashtbl.add r.scope ret 0
            end;
          (** Append [!Apac_macros.goto_label] to the new sequence. *)
          let body = trm_seq_add_last
                       (trm_add_label
                          Apac_macros.goto_label (trm_unit())) body in
          (** Mark the new sequence with [!Apac_macros.candidate_body_mark] *)
          let body = trm_add_mark Apac_macros.candidate_body_mark body in
          (** and with [!Apac_macros.candidate_main_mark] if [f] is the [main]
              function. *)
          let body = if f.name = !Apac_macros.apac_main then
                       trm_add_mark Apac_macros.candidate_main_mark body
                     else
                       body in
          (** If [f] returns something else than [void], we need to declare
              [!Apac_macros.result_variable] at the beginning of the new
              sequence and return its value at the end of the sequence. *)
          let body = if (is_type_unit ty) then
                       trm_seq_nomarks [body]
                     else
                       trm_seq_nomarks [
                           (trm_let_mut (ret, ty) (trm_uninitialized ()));
                           body;
                           trm_ret (Some (trm_var_get ret))
                         ] in
          (** Rebuild and return the function definition term. *)
          trm_let_fun ~annot:t.annot f ty args body
        ) tg)

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
