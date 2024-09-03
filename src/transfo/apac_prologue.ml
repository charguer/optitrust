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
    definition. If the function meets the requirements of a taskification
    candidate or if the function calls a taskification candidate, the pass marks
    the function and its body with [!Apac_macros.candidate_mark] and
    [!Apac_macros.task_group_mark], respectively. To consider a function a
    taskification candidate, it must feature at least two calls to a function
    having a record in [!Apac_records.functions] or one call to such a function
    and one loop or two sibling, i.e. non-nested, loops. *)
let select_candidates (tg : target) : unit =
  (** Initialize a counter [calls] of calls to functions having a record in
      [!Apac_records.functions], a counter [loops] of loops and a flag [sibling]
      stating on the presence of at least two sibling loops in a sequence of
      statements. *)
  let calls = ref 0 in
  let loops = ref 0 in
  let sibling = ref false in
  (** Initialize a set of variables [candidates] to store the variables refering
      to functions meeting the taskification candidate requirements. *)
  let candidates = ref Var_set.empty in
  (** [select_candidates.count t]: counts calls to functions having a record in
      [!Apac_records.functions] and loops in a term [t]. *)
  let rec count (t : trm) : unit =
    match t.desc with
    (** If [t] is a call to a function having a record in
        [!Apac_records.functions], increase [calls]. *)
    | Trm_apps ({ desc = Trm_var (_, f)}, _) when Var_Hashtbl.mem functions f ->
       incr calls
    (** If [t] is a loop, increase [loops] and continue exploring the abstract
        syntax tree of the body [b] of the loop. *)
    | Trm_for (_, b, _)
      | Trm_for_c (_, _, _, b, _)
      | Trm_while (_, b) 
      | Trm_do_while (b, _) -> incr loops; trm_iter count b
    (** Otherwise, simply continue exploring the abstract syntax tree. *)
    | _ -> trm_iter count t
  in
  (** [select_candidates.check t]: checks for the presence of at least two
      sibling loops in a term [t]. *)
  let rec check (t : trm) : unit =
    match t.desc with
    (** If [t] is a sequence of statements [s], check whether the latter
        features at least two sibling loops. *)
    | Trm_seq s ->
       let n = Mlist.fold_left (fun acc e ->
                   match e.desc with
                   | Trm_for _
                     | Trm_for_c _
                     | Trm_while _
                     | Trm_do_while _ -> acc + 1
                   | _ -> acc
                 ) 0 s in
       if n > 1 then
         (** If so, update [sibling]. *)
         sibling := true
       else
         (** Otherwise, check in the statements of [s]. *)
         Mlist.iter (fun e -> trm_iter check e) s
    (** In any other case, continue exploring the abstract syntax tree. *)
    | _ -> trm_iter check t
  in
  (** Iterate over the target function definitions and store each function
      meeting the taskification candidate requirements in [candidates]. *)
  Target.iter_at_target_paths (fun t ->
      (** Deconstruct the definition term [t] of the function [f]. *)
      let error = "Apac_prologue.select_candidates: expected a target to a \
                   function definition!" in
      let (f, _, _, body) = trm_inv ~error trm_let_fun_inv t in
      (** Count calls to functions having a record in [!Apac_records.functions]
          and loops in the [body] of [f]. *)
      count body;
      (** If it contains at least two calls to function having a record in
          [!Apac_records.functions] or one call to such a function and at least
          one loop, add [f] to [candidates]. *)
      if (!calls > 1) || (!calls > 0 && !loops > 0) then
        candidates := Var_set.add f !candidates
      else if (!loops > 1) then
        begin
          (** If it is not the case, but if there are at least two loops in the
              [body] of [f], check the latter for the presence of at least two
              sibling loops. *)
          check body;
          (** It this condition verifies, add [f] to [candidates]. *)
          if !sibling then candidates := Var_set.add f !candidates;
        end;
      (** Reset counters and flags. *)
      calls := 0; loops := 0; sibling := false
    ) tg;
  (** Iterate again over the target function definitions and update [candidates]
      with functions featuring a call to a function already present in
      [candidates], i.e. with functions calling a function meeting the
      taskficiation candidate requirements. *)
  Target.iter_at_target_paths (fun t ->
      (** Deconstruct the definition term [t] of the function [f]. *)
      let error = "Apac_prologue.select_candidates: expected a target to a \
                   function definition!" in
      let (f, _, _, body) = trm_inv ~error trm_let_fun_inv t in
      (** Loop over the [body] of [f] and if [f] features a call to a function
          [f'] in [candidates], add [f] to [candidates]. *)
      let rec loop = fun c ->
        match c.desc with
          | Trm_apps ({ desc = Trm_var (_, f')}, _)
               when Var_set.mem f' !candidates ->
             candidates := Var_set.add f !candidates;
             trm_iter loop c
          | _ -> trm_iter loop c
      in
      loop body
    ) tg;
  (** Iterate one last time over the target function definitions and mark each
      function present in [candidates], i.e. each taskification candidate, and
      its body with [!Apac_macros.candidate_mark] and
      [!Apac_macros.task_group_mark], respectively. *)
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the definition term [t] of the function [f]. *)
      let error = "Apac_prologue.select_candidates: expected a target to a \
                   function definition!" in
      let (f, ty, args, body) = trm_inv ~error trm_let_fun_inv t in
      (** If [f] belongs to [candidates], *)
      if (Var_set.mem f !candidates) then
        (** Add [!Apac_macros.task_group_mark] to the [body] of [f]. *)
        let body = trm_add_mark Apac_macros.task_group_mark body in
        (** Rebuild the function definition with the marked [body] as [t']. *)
        let t' = trm_let_fun ~annot:t.annot ~ctx:t.ctx f ty args body in
        (** Mark [t'] with [!Apac_macros.candidate_mark]. *)
        let t' = trm_add_mark Apac_macros.candidate_mark t' in
        (** Return the resulting function definition [t']. *)
        t'
      else
        (** Otherwise, keep the definition of [f] as is. *)
        t
    ) tg

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
  let res_var = new_var Apac_macros.result_variable in
  let body', _ = Internal.replace_return_with_assign ~check_terminal:false
                   ~exit_label:Apac_macros.goto_label res_var body in
  (* Add the the return variable into the function record, if it exists. This
     may not be the case when testing the transformation separately, for
     example. *)
  if (Var_Hashtbl.mem Apac_records.functions var) then
    begin
      let r = Var_Hashtbl.find Apac_records.functions var in
      Var_Hashtbl.add r.scope res_var 0
    end;
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
