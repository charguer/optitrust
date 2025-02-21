open Ast
open Trm
open Path
open Target
open Mark
open Apac_macros
open Apac_dep
open Apac_tasks

(** [trmq]: a persistent FIFO queue of terms. *)
type trmq = trm Queue.t

(** [varq]: a persistent FIFO queue of variables. *)
type varq = (var * trm) Queue.t

(** [depscope]: a hash table of dependencies to their location in a task
    graph. *)
type depscope = (TaskGraph.V.t * TaskGraph.t) Dep_hashtbl.t

(** [substack]: a stack of array subscript-related dependencies with their last
    write location. *)
type substack = (Dep.t * TaskGraph.V.t * TaskGraph.t) Stack.t

(** [count_update ~backend postamble]: generates the portion of the
    instrumentation code allowing to update the task count [ApacCount] variable.
    If [postamble] is [false], it produces code to increment the counter when a
    new task is about to be spawned. In order to produce code to decrement the
    counter at the end of a task, set [postamble] to [true]. *)
let count_update (postamble : bool) : trm =
  (** Retrieve the string representation of the involved instrumentation
      variables. *)
  let count = get_apac_variable ApacCount in
  let ok = get_apac_variable ApacCountOk in
  (** Decide on the update operation to apply. *)
  let op = if postamble then "--" else "++" in
  (** Build the task count update term. *)
  let update = code (Instr (count ^ op)) in
  (** Prepend it with the OpenMP atomic pragma. *)
  let update = trm_add_pragma (Atomic None) update in
  (** Put the update statement into a sequence. *)
  let update = trm_seq_nomarks [update] in
  (** Convert the string representation of [ok] to a term prior to *)
  let condition = code (Expr ok) in
  (** building the final if-conditional. *)
  trm_if condition update (trm_unit ())

(** [depth_update]: generate the portion of the instrumentation code allowing
    to update the task depth [ApacDepth] variable at the beginning of a newly
    spawned task. *)
let depth_update () : trm =
  (** Retrieve the string representation of the involved instrumentation
      variables. *)
  let count = get_apac_variable ApacDepth in
  let local = get_apac_variable ApacDepthLocal in
  (** Build the task depth increment term. *)
  let increment = code (Instr (count ^ " = " ^ local ^ " + 1")) in
  (** Put the increment term into a sequence. *)
  let increment = trm_seq_nomarks [increment] in
  (** Build the boolean expression deciding whether to update [ApacDepth] or
      not prior to *)
  let condition =
    let ok = get_apac_variable ApacDepthOk in
    if !Apac_flags.cutoff_count then
      code (Expr ((get_apac_variable ApacCountOk) ^ " || " ^ ok))
    else
      code (Expr ok)
  in
  (** building the associated if-conditional. *)
  trm_if condition increment (trm_unit ())

(** [get_cutoff_count ()]: generates condition term for the cut-off mechanism
    based on the count of submitted tasks. *)
let get_cutoff_count () : trm =
  trm_var (new_var (get_apac_variable ApacCountOk))

(** [get_cutoff_depth ()]: generates condition term for the cut-off mechanism
    based on the per-thread parallelism depth. *)
let get_cutoff_depth () : trm =
  trm_var (new_var (get_apac_variable ApacDepthOk))

(** [get_cutoff_count_and_depth ()]: generates condition term for both the
    cut-off mechanism based on the count of submitted tasks and the the cut-off
    mechanism based on the per-thread parallelism depth. *)
let get_cutoff_count_and_depth () : trm =
  trm_or (get_cutoff_count ()) (get_cutoff_depth ())

(** [get_cutoff_execution_time f]: generates condition term for the cut-off
    mechanism based on the execution time estimation of a task while considering
    the execution time computation formula [f]. *)
let get_cutoff_execution_time (f : trm) : trm =
  trm_gt f (trm_var (new_var (get_apac_variable ApacCutOff)))

(** [cutoff_task_candidate p t]: surround the task candidate term [t] with the
    statements ensuring the granularity control based on the cut-off mechanism
    based on the count of submitted tasks and the the cut-off mechanism based on
    the per-thread parallelism depth. The function produces the required
    statements based on the flags [!Apac_flags.cutoff_count] and
    [!Apac_flags.cutoff_depth]. *)
let cutoff_task_candidate (p : cpragma) (t : trm) : trm =
  match (!Apac_flags.cutoff_count, !Apac_flags.cutoff_depth) with
  | (true, true) ->
     Syntax.trm_seq_no_brace [
         count_update false;
         trm_add_pragma
           p (trm_seq_nomarks [depth_update (); t; count_update true])
       ]
  | (true, false) ->
     Syntax.trm_seq_no_brace [
         count_update false;
         trm_add_pragma
           p (trm_seq_nomarks [t; count_update true])
       ]
  | (false, true) ->
     trm_add_pragma
       p (trm_seq_nomarks [depth_update (); t])
  | (false, false) ->
     trm_add_pragma p t

let subst_pragmas (va : var) (tv : trm)
      (pl : cpragma list) : cpragma list =
  let aux (dl : deps) : deps =
    let v0 = Val_lit (Lit_int 0) in
    let v0 = trm_val v0 in
    List.map(fun d ->
        match d with
        | Dep_trm (t, v) ->
           let tv' = match t.desc with
             | Trm_var (_, v') when v' = va ->
                trm_array_get tv v0
             | _ -> trm_get tv
           in
           let t' = trm_subst_var va tv' t in
           Dep_trm (t', v)
        | Dep_var v when v = va ->
           let t' = trm_array_get tv v0 in
           Dep_trm (t', v)
        | _ -> d) dl
  in
  List.map (fun p ->
      let cl' = match p with
        | Task cl
          | Taskwait cl -> cl
        | _ -> []
      in
      let cl' = if cl' <> [] then
                  List.map (fun c ->
                      match c with
                      | Depend dl ->
                         let io : deps ref = ref [] in
                         let dl' = List.map (fun dt ->
                                       match dt with
                                       | In dl -> io := !io @ dl; In (aux dl)
                                       | Inout dl ->
                                          io := !io @ dl; Inout (aux dl)
                                       | _ -> dt) dl in
                         io := List.filter (fun d ->
                                   match d with
                                   | Dep_trm (_, v) when v = va -> true
                                   | Dep_var v when v = va -> true
                                   | _ -> false) !io;
                         let dl' = List.map (fun dt ->
                                       match dt with
                                       | In dl -> In (dl @ !io)
                                       | _ -> dt) dl' in
                         Depend dl'
                      | If e ->
                         let v0 = Val_lit (Lit_int 0) in
                         let v0 = trm_val v0 in
                         let tv' = match e.desc with
                           | Trm_var (_, v') when v' = va ->
                              trm_array_get (trm_get tv) v0
                           | _ -> trm_get tv
                         in
                         If (trm_subst_var va tv' e)
                      | _ -> c) cl'
                else []
      in
      match p with
      | Task _ -> Task cl'
      | Taskwait _ -> Taskwait cl'
      | _ -> p) pl

(** [heapify_on t]: see [!Apac_parallelization.heapify]. *)
let heapify_on (t : trm) : trm =
  let open Typ in
  let open Apac_miscellaneous in
  let variable (t : trm) : bool =
    trm_fold (fun acc t ->
        match t.desc with
        | Trm_var _ -> acc || true
        | _ -> acc
      ) false t
  in
  (** [heapify_on.typ_has_const ty]: checks whether the type [ty] features a
      constant type (see [!type:ty]). *)
  let rec typ_has_const (ty : typ) : bool =
    match ty.typ_desc with
    | Typ_const _ -> true
    | Typ_ptr { ptr_kind = _; inner_typ = ty } -> typ_has_const ty
    | Typ_array (ty, _) -> typ_has_const ty
    | Typ_constr _ when typ_is_alias ty ->
      begin match typ_get_alias ty with
      | Some ty -> typ_has_const ty
      | None -> false
      end
    | _ -> false
  in
  (** [heapify_on.one reference deletes v ty init]: promotes a single variable
      declaration, represented by the variable [v] of type [ty] and by the
      initialization term [init], from the stack to the heap. The [reference]
      flag tells us whether we are declaring a reference. The [deletes] queue
      stores the [delete] terms we generate to deallocate the variables we
      promote to heap. *)
  let one (reference : bool) (deletes : trmq) (variables : varq)
        (vk : varkind) (v : var) (ty : typ) (init : trm) : typed_var * trm =
    (** Beyond promoting variables to the heap, this function allow for
        producing an adequate [delete] term allowing for future de-allocation of
        the promoted variables. However, the promotion to the heap does not
        always take place, i.e. if the target variable is already on the heap.
        At the end of the process, the [delete] integer will tell us whether the
        production of a [delete] term is required (positive value) or not (zero)
        and whether we are freeing a variable (1) or an array (2). *)
    let delete = ref 1 in
    (** Depending on whether it is a multiple [mult] is [true] or a single
        variable declaration [mult] is [false], the encoding of the type and the
        const status in the OptiTrust AST differs. *)
    let result =
      (** Acquire the inner type of [ty], e.g. [int] from [int *]. *)
      let tyi = get_inner_ptr_type ty in
      (** If we are declaring a reference, *)
      if reference then
        (** we have to apply a get operation on the initialization term.
            Otherwise, OptiTrust shall add a [&] operator to the latter, e.g.
            [const int &b = 1] would become [const int &b = &1]. *)
        let init2 = trm_get init in
        begin
          (** Here, we have to do something only when it is a constant reference
              to a literal value, i.e. it is not referencing a previously
              user-allocated memory location. *)
          if is_typ_const tyi && not (variable init) then
            (** [ty] is already a reference. We only have to constify it. *)
            ((v, typ_const ty), trm_new tyi init2)
          else
            begin
              delete := 0;
              (** The above affirmation is true only in the case of constant
                  references. For mutable references, we have to restore the
                  reference type of [tyi]. Otherwise, a [int &e = i] results in
                  [int * e = i] even if the heapification is not performed. *)
              ((v, typ_ptr Ptr_kind_ref tyi), init2)
            end
        end
      else
        (** Otherwise, we distinguish the following four different cases. *)
        begin
          (** + The value as well as the memory location are [const], e.g. in
              [int const * a = &i, * const b = &j] it is the case of [b]. *)
          if is_typ_const ty then
            begin
              (** In the case of a simple variable declaration, we acquire the
                  inner const type directly from [ty] unlike in the case of a
                  multiple variable declaration (see above). *)
              let tyc = get_inner_const_type ty in
              (** If the variable is already a pointer, we do not need to
                  transform it to a pointer type. *)
              let ty2 =
                if is_typ_ptr tyc then ty else typ_ptr Ptr_kind_mut ty in
              (** Then, we have to restore the [const] status of the variable,
                  if needed. *)
              let ty2 = if is_typ_const ty2 then ty2 else typ_const ty2 in
              (** If the variable is a pointer, we consider that it already
                  points to some data in the heap. If it is not the case, e.g.
                  in [int i = 1; int * a = &i], it is not of the responsibility
                  of this transformation function. Otherwise, we replace the
                  initial [init] an adequate allocation term.

                  Note that, here, we perform the test on the inner type because
                  [ty] is a [const] type in this case. *)
              let init2 =
                if is_typ_ptr tyc then
                  begin
                    delete := 0;
                    init
                  end
                else trm_new ty init in
              (** Return the updated variable declaration and definition. *)
              ((v, ty2), init2)
            end
          (** + We are declaring a static array (of values, of pointers, [const]
              or not), e.g. [int * a[10]]. *)
          else if is_typ_array tyi then
            begin
              (** To transform a static array allocation to a dynamic array
                  allocation, we need to determine its inner type, i.e. the type
                  of the values stored in the array (without the square
                  brackets), for the construction of the [new <inner-type>]
                  allocation term. Note that if the static variable is an array
                  of constants, e.g. [int const tab[2]], [tya] shall be [const
                  int]. *)
              let tya = get_inner_array_type tyi in
              (** We then transform the lvalue type to a pointer, e.g. [int
                  tab[2]] becomes [int * tab]. *)
              let ty2 = typ_ptr Ptr_kind_mut tya in
              (** If it is an array of constants, we also need to add [const] to
                  [const int * tab] so as it becomes [const int * const tab]. *)
              let ty2 = if is_typ_const tya then typ_const ty2 else ty2 in
              (** The transformation of [init] to an adequate allocation term is
                  required only in the case of an array of constants. Otherwise,
                  it is done implicitly by OptiTrust. *)
              delete := 2;
              let init2 =
                if is_typ_const tya then trm_new ty init else init in
              (** Return the updated variable declaration and definition. *)
              ((v, ty2), init2)
            end
          (** + We are declaring something that is already a pointer. In this
              case, we do not transform the variable declaration. *)
          else if is_typ_ptr tyi then
            begin
              (** However, for some reason, the original allocation term becomes
                  encompassed by a pointer allocation term at some point, so we
                  have to gather the original one back. *)
              let error = "Apac_parallelization.heapify_on.one: expected a \
                           nested allocation term." in
              let (_, init2) = trm_inv ~error trm_new_inv init in
              delete := 0;
              ((v, tyi), init2)
            end
          (** + Any other cases. Typically, we refer here to static variable
              declarations and definitions such as [int a = 1] or to variable
              declarations and definitions that were not fully constified, i.e.
              where either the value or the memory location is not [const] or
              none of the two. *)
          else
            begin
              (** We then transform the lvalue type to a pointer, e.g. [int a]
                  becomes [int * a]. *)
              let ty2 = typ_ptr Ptr_kind_mut tyi in
              (** Then, we have to restore the [const] status of the variable if
                  it was present in the original inner type [tyi]. *)
              let ty2 = if is_typ_const tyi then typ_const ty2 else ty2 in
              (** The transformation of [init] to an adequate allocation term is
                  required only in the case of an array of constants. Otherwise,
                  it is done implicitly by OptiTrust. *)
              let init2 =
                if is_typ_const tyi then
                  trm_new ty init
                else
                  begin
                    delete := 1;
                    init
                  end
              in
              (** Return the updated variable declaration and definition. *)
              ((v, ty2), init2)
            end
        end
    in
    (** If necessary, produce [delete] term allowing for deallocating the
        promoted variable and add them to the [deletes] queue. *)
    if !delete > 0 then
      begin
        let inout = Dep_var v in
        let inout = [Inout [inout]] in
        let depend = [Depend inout] in
        let clauses = [Default Shared_m] in
        let clauses = clauses @ depend in
        let clauses =
          let fp () =
            [FirstPrivate
               [new_var (Apac_macros.get_apac_variable ApacDepthLocal)]]
          in
          match (!Apac_flags.cutoff_count, !Apac_flags.cutoff_depth) with
          | (true, true) ->
             clauses @ (fp ()) @ [If (get_cutoff_count_and_depth ())]
          | (true, false) ->
             clauses @ [If (get_cutoff_count ())]
          | (false, true) ->
             clauses @ (fp ()) @ [If (get_cutoff_depth ())]
          | (false, false) ->
             clauses
        in
        let vt = trm_var ~kind:vk v in
        let vt' = if typ_has_const ty then vt else trm_get vt in
        let dt =
          cutoff_task_candidate (Task clauses) (trm_delete (!delete = 2) vt') in
        Queue.add dt deletes;
        if !delete <> 2 then Queue.add (v, vt) variables;
      end;
    result
  in
  (** [Apac_basic.heapify_on.delete deletes level t]: an auxiliary function to
      place the [delete] terms from [deletes] in [t] following the rules below:

      - before the [return] term of the current sequence term [t] or the end of
      the current sequence if no [return] term is present,
      - before each [return] term in the scope of the current sequence term [t],
      - before [break] and [continue] statements in the current sequence term
      [t] if it is the body of a loop or a case of a [switch] statement (see the
      [breakable] flag). *)
  let rec delete (deletes : trms) (level : int) (t : trm) : trm =
    match t.desc with
    | Trm_seq _ ->
       let breakable = trm_has_mark Apac_macros.heapify_breakable_mark t in
       let t' = trm_map (delete deletes (level + 1)) t in
       let error =
         "Apac_parallelization.heapify_on.delete: expected a sequence term." in
       let stmts = trm_inv ~error trm_seq_inv t' in
       let stmts' = match Mlist.findi (fun t -> is_trm_abort t) stmts with
         | Some (idx, abort) when is_return abort || breakable ->
            let abort' = Syntax.trm_seq_no_brace (deletes @ [abort]) in
            Mlist.replace_at idx abort' stmts
         | _ when level < 1 ->
            let len = Mlist.length stmts in
            Mlist.insert_sublist_at len deletes stmts
         | _ -> stmts
       in
       trm_seq ~annot:t.annot stmts'
    | _ -> t
  in
  (** Deconstruct the sequence term [t] into the [Mlist] of terms [ml]. *)
  let error =
    "Apac_parallelization.heapify_on: expected a target to a sequence." in
  let ml = trm_inv ~error trm_seq_inv t in
  (** Initialize a queue for the [delete] terms allowing for deallocating the
      promoted variables.*)
  let deletes = Queue.create () in
  (** Initialize a queue for the deleted variables.*)
  let variables = Queue.create () in
  (** Initialize a queue storing a set of declared variables for each statement
      in [ml]. *)
  let declarations = Queue.create () in
  (** Map over the terms in [ml] and perform the heapification of variable
      declarations. *)
  let ml = Mlist.map (fun t ->
               match t.desc with
               | Trm_let (kind, (v, ty), init, _) ->
                  (** Store the declared variable [v] in [declarations]. *)
                  Queue.push (Var_set.singleton v) declarations;
                  let ((v2, ty2), init2) =
                    one (trm_has_cstyle Reference t) deletes
                      variables kind v ty init in
                  trm_pass_pragmas t (trm_let kind (v2, ty2) init2)
               | _ ->
                  (** [t] is not a declaration. There is nothing to store in
                      [declarations] for this statement. *)
                  Queue.push (Var_set.empty) declarations; t
             ) ml in
  (** Transform the [deletes] queue into a list. *)
  let deletes = List.of_seq (Queue.to_seq deletes) in
  (** Transform the [variables] queue into a list. *)
  let variables = List.of_seq (Queue.to_seq variables) in
  (** Transform the [declarations] queue into a list. *)
  let declarations = List.of_seq (Queue.to_seq declarations) in
  (** Update occurrences of [variables] in the pragmas of each statement [t] in
      [ml], if any. *)
  let ml = Mlist.mapi (fun i t ->
               let ds = List.nth declarations i in
               List.fold_left (fun acc (v, tv) ->
                   (** However, do not update the occurrences of variables being
                       declared in [t]. Indeed, the declaration of a variable
                       [v] simply generates an inout-dependency on [v] whether
                       it is a pointer or not. When we promote a variable from
                       the stack to the heap, it becomes a pointer and the
                       pragma update would result in splitting the dependency in
                       two, i.e. an inout-dependency on [v\[0\]] and an
                       in-dependency on [v]. We do not want this, so we do not
                       perform the pragma update in the case of variables being
                       declared in [t]. *)
                   if not (Var_set.mem v ds) then
                     apply_on_pragmas (fun pl -> (subst_pragmas v tv) pl) acc
                   else acc
                 ) t variables) ml in
  (** Re-build the sequence term. *)
  let t' = trm_seq ~annot:t.annot ml in
  let t' = List.fold_left (fun acc (v, tv) ->
               trm_subst_var v (trm_get tv) acc
             ) t' variables in
  (** Add the [delete] terms from [deletes] to [t']. *)
  delete deletes 0 t'

(** [heapify tg]: expects the target [tg] to point at a sequence in which it
    promotes variables delacred on the stack to the heap. It applies on each
    simple and on each multiple variable declaration that is not a reference or
    a pointer to a previously user-allocated memory location.

    Example:

    {[int tab\[5\] = { 1, 2, 3, 4, 5 };]}

    becomes:

    {[int * tab = new int\[5\] { 1, 2, 3, 4, 5 }]}

    However, in:

    {[
    int * a = new int(10);
    int &b = &a;
    ]}

    nor [a] nor [b] are transformed.

    As we will have to free the variables promoted to the heap at some point,
    this transformation also places adequate [delete] terms at the right
    places. *)
let heapify (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
      Target.apply_at_target_paths heapify_on tg)

(* [synchronize_subscripts_on p t]: see [synchronize_subscripts]. *)
let synchronize_subscripts_on (p : path) (t : trm) : unit =
  let rec synchronize (scope : depscope) (subscripts : substack)
            (graph : TaskGraph.t) (vertex : TaskGraph.V.t) : unit =
    let task = TaskGraph.V.label vertex in
    let subscripted = Dep_set.filter (fun d ->
                          Dep_map.has_with_attribute
                            d Accessor task.ioattrs
                        ) task.ins in
    if (Dep_set.cardinal subscripted) > 0 then
      begin
        Dep_set.iter (fun d ->
            if (Dep_hashtbl.mem scope d) then
              begin
                let (v, g) = Dep_hashtbl.find scope d in
                Stack.push (d, v, g) subscripts
              end
          ) subscripted;
        task.ioattrs <- Dep_map.remove_attribute Accessor task.ioattrs
      end;
    if (Task.attributed task Taskifiable) then
      Dep_set.iter (fun d ->
          Dep_hashtbl.add scope d (vertex, graph)
        ) task.inouts;
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter
              (synchronize (Dep_hashtbl.copy scope) subscripts go) go
          ) gl
      ) task.children
  in
  let process (d : Dep.t) (v : TaskGraph.V.t) (g : TaskGraph.t) : unit =
    let succ = TaskGraph.fold_succ (fun s a -> a @ [s]) g v [] in
    let v' = TaskGraph.V.label v in
    if !Apac_flags.verbose then
      Printf.printf "Synchronizing subscripts, processing successors of the \
                     task `%s'.\n" (Task.to_string v');
    if (List.length succ) > 0 then
      begin
        let first = List.hd succ in
        let task = TaskGraph.V.label first in
        task.ins <- Dep_set.add d task.ins;
        task.ioattrs <-
          Dep_map.add d (DepAttr_set.singleton Accessor) task.ioattrs
      end
  in
  (** Find the parent function [f]. *)
  let f = match (Apac_miscellaneous.find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_epilogue.synchronize_subscripts_on: unable to \
                          find parent function. Task group outside of a \
                          function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find Apac_records.functions f in
  let scope = Dep_hashtbl.create 97 in
  let subscripts = Stack.create () in
  TaskGraphTraverse.iter (synchronize scope subscripts r.graph) r.graph;
  Stack.iter (fun (d, v, g) -> process d v g) subscripts;
  (** Dump the resulting task candidate graph, if requested. *)
  if !Apac_flags.verbose then
    begin
      Printf.printf "Task candidate graph of `%s' (sync. subscripts):\n"
        (var_to_string f);
      TaskGraphPrinter.print r.graph
    end;
  if !Apac_flags.keep_graphs then
    TaskGraphExport.to_pdf r.graph (Apac_macros.gf ~suffix:"subscripts" f)

let synchronize_subscripts (tg : target) : unit =
  Target.iter (fun t p ->
      synchronize_subscripts_on p (Path.get_trm_at_path p t)) tg

(** [place_barriers_on p t]: see [place_barriers]. *)
let place_barriers_on (p : path) (t : trm) : unit =
  (** [place_barriers_on.find g]: looks in the task candidate graph [g] for task
      candidates immediately preceding the task candidate which is either itself
      the first eligible task candidate or the first task candidate to involve
      an eligible task candidate (in a nested task candidate graph). *)
  let find (g : TaskGraph.t) : TaskGraph.V.t list =
    (** [place_barriers_on.find.taskifiable v]: checks whether the task
        candidate vertex [v] is an eligible task candidate or whether it
        involves an eligible task candidate in a nested task candidate graph. *)
    let rec taskifiable (v : TaskGraph.V.t) : bool =
      let t = TaskGraph.V.label v in
      if (Task.attributed t Taskifiable) then true
      else
        List.fold_left (fun al gl ->
            List.fold_left (fun ao go ->
                TaskGraph.fold_vertex (fun v' a ->
                    a || (taskifiable v')
                  ) go false
              ) false gl
          ) false t.children
    in
    (** [place_barriers_on.find.core p l]: core function operating on a list [l]
        of the task candidate vertices of [g]. [p] represents the previously
        visited vertices (empty in the initial call to the function). *)
    let rec core (p : TaskGraph.V.t list)
              (l : TaskGraph.V.t list) : TaskGraph.V.t list =
      match l with
      | t :: tl -> if (taskifiable t) then p else core (t :: p) tl
      | [] -> p
    in
    (** Retrieve all the vertices of [g] in a list following the ascending order
        of their schedules (see [Task.t]). *)
    let vs = TaskGraphTraverse.fold g in
    (** Reverse the list so as to follow the schedules in descending order. *)
    let vs = List.rev vs in
    (** This way, the task candidate immediately preceding the first eligible
        task candidate in [vs] amounts to the task candidate immediately
        following the last eligible task candidate in [g]. *)
    core [] vs 
  in
  (** [place_barriers_on.tasks ts v]: auxiliary function to explore each task
      candidate vertex [v] of the task candidate graph [g] in search for
      eligible task candidates to store into the task candidate stack [ts]. *)
  let rec tasks (ts : (Task.t * TaskGraph.V.t) Stack.t) (g : TaskGraph.t)
            (v : TaskGraph.V.t) : unit =
    (** Retrieve the label [t] of the task candidate [v]. *)
    let t : Task.t = TaskGraph.V.label v in
    (** If [t] indicates that [v] is an eligible task candidate, i.e. it carries
        the [Taskifiable] attribute, add it to the stack of preceding eligible
        task candidates [ts]. *)
    if (Task.attributed t Taskifiable) then
      Stack.push (t, v) ts;
    (** Explore the task candidates in nested task candidate graphs, if any. *)
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter_schedule (tasks ts go) go
          ) gl
      ) t.children
  in
  (** [place_barriers_on.process l c s ts v]: auxiliary function for placing a
      barrier in front of a vertex [v] depending on the presence of preceding
      eligible task candidates in the stack [ts]. [l] represents the list of
      task candidates immediately following the last eligible task candidate in
      the task candidate graph [g] we apply the function on. When [v] equals
      [p], place a global synchronization barrier on the latter and set [c] to
      [true] so as to prevent the function from placing further barriers. When
      processing a loop nest, we do not consider only preceding eligible task
      candidates but all the eligible task candidates of its outermost loop. To
      this end, we pre-load [ts] with all the eligible task candidates from [v]
      and its nested task candidate graphs before recursively processing it. In
      this case, we set the [s] flag to [true] so as to prevent the function
      from pushing eligible task candidates from [v] into [ts] twice. *)
  let rec process (l : TaskGraph.V.t list) (c : bool ref) (s : bool)
            (ts : (Task.t * TaskGraph.V.t) Stack.t) (g : TaskGraph.t)
            (v : TaskGraph.V.t) : unit =
    (** Retrieve the label [t] of the task candidate [v]. *)
    let t : Task.t = TaskGraph.V.label v in
    (** If [t] indicates that [v] is an eligible task candidate, i.e. it carries
        the [Taskifiable] attribute, and if we are not in a loop, i.e. [s] is
        [false], add [t] to [ts]. *)
    if (Task.attributed t Taskifiable) then
      begin
        if not s then Stack.push (t, v) ts
      end
    else if (TaskGraph.V.equal (List.hd l) v) then
      begin
        (** If [v] is the first task candidate in [l] immediately following the
            last eligible task candidate in the target task candidate graph and
            if any task candidate [e] from the list [l] depends on any preceding
            eligible task candidate [e'] from the stack [ts], *)
        let depend =
          List.fold_left (fun acc e ->
              let e : Task.t = TaskGraph.V.label e in
              acc || (Stack.fold (fun acc' (e', v') ->
                          if (TaskGraph.mem_vertex g v) &&
                               (TaskGraph.mem_vertex g v') then
                            acc' || (Task.depending e' e)
                          else
                            let ue' = Task.copy e' in
                            ue'.ioattrs <-
                              Dep_map.remove_attribute Subscripted ue'.ioattrs;
                            acc' || (Task.depending ue' e)
                        ) false ts)
            ) false l
        in
        if depend then
          (** place a global synchronization barrier on [v] and *)
          t.attrs <- TaskAttr_set.add WaitForAll t.attrs;
        (** set [c] to [true] so as to prevent the function from placing further
            barriers. *)
        c := true
      end
      else if (Stack.length ts) > 0 && !c = false then
        (** Otherwise, if, according to [t], [v] does not represent a global
            synchronization barrier (see the [WaitForAll] attribute) nor
            [Apac_macros.goto_label] (see the [ExitPoint] attribute) nor a jump
            to the latter (see the [IsJump] attribute), we have to determine
            whether [v] depends on a preceding eligible task candidate, if
            any. *)
        if not (Task.attributed t WaitForAll) &&
             not (Task.attributed t ExitPoint)  &&
               not (Task.attributed t IsJump) then
          begin
            (** To this end, we imagine a temporary task candidate with a label
                [temp], based on [t], but omitting the dependencies from nested
                task candidate graphs, if any, as well as the dependencies on
                new variables. Indeed, a variable must not appear in a pragma
                before being declared. We process the task candidates in nested
                task candidate graphs separately through a recurive call to this
                function (see below). *)
            let temp = Task.copy t in
            temp.ins <- if t.children <> [[]] then
                          Dep_set.filter (fun d ->
                              Dep_map.has_with_attribute d Condition t.ioattrs
                            ) t.ins
                        else t.ins;
            temp.inouts <- if t.children <> [[]] then
                             Dep_set.filter (fun d ->
                                 (Dep_map.has_with_attribute
                                    d Condition t.ioattrs)
                               ) t.inouts
                           else t.inouts;
            temp.inouts <- Dep_set.filter (fun d ->
                               not (Dep_map.has_with_attribute
                                      d NewVariable temp.ioattrs)
                             ) temp.inouts;
            (** Check whether the temporary task candidate shares a data
                dependency with a preceding eligible task candidate. *)
            let depends =
              Stack.fold (fun acc (task', v') ->
                  (** Note that we must filter out dependencies on new variables
                      from the preceding task candidates too. *)
                  let task'' = Task.copy task' in
                  task''.inouts <-
                    Dep_set.filter (fun d ->
                        not (Dep_map.has_with_attribute
                               d NewVariable task''.ioattrs)
                      ) task''.inouts;
                  if not (TaskGraph.mem_vertex g v') then
                    task''.ioattrs <-
                      Dep_map.remove_attribute Subscripted task''.ioattrs;
                  acc || (Task.depending task'' temp)
                ) false ts in
            (** If so, we request a synchronization barrier for [v] through [t]
                thanks to the [WaitForSome] attribute. *)
            if depends then t.attrs <- TaskAttr_set.add WaitForSome t.attrs
          end
        else if (Task.attributed t IsJump) then
          (** However, a jump to [Apac_macros.goto_label] (see the [IsJump]
              attribute) requires a global synchronization barrier if any
              eligible task candidate precedes it in the task candidate
              graph. *)
          begin
            t.attrs <- TaskAttr_set.add WaitForAll t.attrs
          end;
    (** Process the task candidates in nested task candidate graphs, if any. *)
    List.iter2 (fun ct gl ->
        (** If [t] features a loop, *)
        let s = match ct.desc with
          | Trm_for_c _
            | Trm_for _
            | Trm_while _
            | Trm_do_while _ ->
             (** pre-load [ts] with all the eligible task candidates from
                 within the body of the loop. *)
             List.iter (fun go ->
                 TaskGraphTraverse.iter_schedule (tasks ts go) go
               ) gl;
             true
          | _ -> false
        in
        List.iter (fun go ->
            TaskGraphTraverse.iter_schedule (process l c s ts go) go
          ) gl
      ) t.current t.children
  in
  (** Find the parent function [f]. *)
  let f = match (Apac_miscellaneous.find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_epilogue.place_barriers_on: unable to find \
                          parent function. Task group outside of a function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find Apac_records.functions f in
  (** Based on schedules, identify the task candidate immediately following the
      last eligible task candidate in the task candidate graph [r.graph] of [f].
      To this end, iterate over the task candidates of [r.graph], including the
      those from nested task candidate graphs, in descending schedule order (see
      [Task.t]). *)
  let bl : TaskGraph.V.t list = find r.graph in
  if bl <> [] then
    begin
      (** Initialize a stack of preceding eligible task candiates. *)
      let ts : (Task.t * TaskGraph.V.t) Stack.t = Stack.create () in
      (** Process each task candidate in [r.graph]. *)
      let stop = ref false in
      TaskGraphTraverse.iter_schedule
        (process bl stop false ts r.graph) r.graph;
      (** Dump the resulting task candidate graph, if requested. *)
      if !Apac_flags.verbose then
        begin
          Printf.printf "Task candidate graph of `%s' (with barriers):\n"
            (var_to_string f);
          TaskGraphPrinter.print r.graph
        end;
      if !Apac_flags.keep_graphs then
        TaskGraphExport.to_pdf r.graph (Apac_macros.gf ~suffix:"barriers" f)
    end
  else
    let error = Printf.sprintf
                  "Apac_epilogue.place_barriers_on: no eligible task \
                   candidates. There is nothing to do for the function '%s'. "
                  (var_to_string f) in
    fail t.loc error

(** [place_barriers tg]: expects the target [tg] to point at the body of a
    function having a task candidate graph representation. If a vertex in the
    latter does not represent an eligible task candidate, i.e. it does not carry
    the [Taskifiable] attribute, this transformation translates it into a
    synchronization barrier if necessary, i.e. if it depends on a preceding
    eligible task candidate in the task candidate graph. *)
let place_barriers (tg : target) : unit =
  Target.iter (fun t p -> place_barriers_on p (Path.get_trm_at_path p t)) tg

(** [place_task_group tg]: expects the target [tg] to point at a sequence. It
    transforms the sequence into an OpenMP task group.

    For example, the sequence

    {[
    {
      int a;
      f();
      return 0;
    }
    ]}

    becomes

    {[
    #pragma omp taskgroup
    {
      int a;
      f();
      return 0;
    }
    ]}

    However, if the target sequence carries [!Apac_macros.candidate_main_mark],
    the transformation creates a master task group as follows.

    {[
    #pragma omp parallel
    #pragma omp master
    #pragma omp taskgroup
    {
      int a;
      f();
      return 0;
    }
    ]} *)
let place_task_group (tg : target) : unit =
  Target.apply_at_target_paths (fun t ->
      (** Check for the presence of [!Apac_macros.candidate_main_mark] and draw
          the list of pragmas to apply. *)
      let pragmas = if (trm_has_mark Apac_macros.candidate_main_mark t) then
                      [Parallel []; Master ; Taskgroup] else
                      [Taskgroup] in
      (** Apply the pragmas on the target sequence. *)
      trm_add_pragmas pragmas t
    ) tg

(** [clear_marks ()]: clears all the marks we use during the compilation. *)
let clear_marks () : unit =
  Marks.remove Apac_macros.candidate_mark [
      nbAny;
      cMark Apac_macros.candidate_mark
    ];
  Marks.remove Apac_macros.candidate_main_mark [
      nbAny;
      cMark Apac_macros.candidate_main_mark
    ];
  Marks.remove Apac_macros.candidate_body_mark [
      nbAny;
      cMark Apac_macros.candidate_body_mark
    ];
  Marks.remove Apac_macros.heapify_mark [
      nbAny;
      cMark Apac_macros.heapify_mark
    ];
  Marks.remove Apac_macros.heapify_breakable_mark [
      nbAny;
      cMark Apac_macros.heapify_breakable_mark
    ]

let codegen_openmp (v : TaskGraph.V.t) : trms =
  let t = TaskGraph.V.label v in
  if (Task.attributed t WaitForAll) then
    begin
      let pragma = Taskwait [] in
      let first = List.hd t.current in
      let first = trm_add_pragma pragma first in
      first :: (List.tl t.current)
    end
  else if (Task.attributed t Taskifiable) ||
            (Task.attributed t WaitForSome) then
    begin
      if (Task.attributed t Taskifiable) then
        begin
          let gd = Dep_set.filter (fun d ->
                     Dep_map.has_with_attribute d GlobalVariable t.ioattrs
                   ) t.inouts in
          Dep_set.iter (fun d ->
              let v = Dep.variable d in
              let (ty, _) = Var_map.find v !Apac_records.globals in
              Apac_records.globals :=
                Var_map.add v (ty, true) !Apac_records.globals
            ) gd;
          List.iter (fun t ->
              let rec callees = fun s t ->
                match t.desc with
                | Trm_apps ({ desc = Trm_var (_ , f); _ }, _) when
                       Var_Hashtbl.mem Apac_records.functions f &&
                         not (Var_set.mem f s) ->
                   let r = Var_Hashtbl.find Apac_records.functions f in
                   trm_fold callees (Var_set.add f s) r.ast 
                | _ -> s
              in
              let fs = trm_fold callees Var_set.empty t in
              Var_set.iter (fun f ->
                  let r = Var_Hashtbl.find Apac_records.functions f in
                  Var_set.iter (fun v ->
                      let (ty, _) = Var_map.find v !Apac_records.globals in
                      Apac_records.globals :=
                        Var_map.add v (ty, true) !Apac_records.globals
                    ) r.writes
                ) fs
            ) t.current
        end;
      let sync = Dep_set.filter (fun d ->
                     Dep_map.has_with_attribute d Accessor t.ioattrs
                   ) t.ins in
      let sync = Dep_set.to_list sync in
      let sync = if (List.length sync) < 1 then [] else [Depend [In sync]] in
      let (firstprivate, ins') =
        if (Task.attributed t WaitForSome) then
          if t.children <> [[]] then
            (Dep_set.empty,
             Dep_set.filter (fun d ->
                 (Dep_map.has_with_attribute d Condition t.ioattrs) &&
                   not (Dep_map.has_with_attribute
                          d InductionVariable t.ioattrs)
               ) t.ins)
          else
            (Dep_set.empty, t.ins)
        else
          Dep_set.partition (fun d ->
              Dep_map.has_with_attribute d InductionVariable t.ioattrs
            ) t.ins
      in
      let ins' = Dep_set.to_list ins' in
      let ins' = if (List.length ins') < 1 then [] else [In ins'] in
      let inouts' = if (Task.attributed t WaitForSome) && t.children <> [[]]
                    then
                      Dep_set.filter (fun d ->
                          Dep_map.has_with_attribute d Condition t.ioattrs
                        ) t.inouts
                    else t.inouts in
      let inouts' = Dep_set.filter (fun d ->
                        not (Dep_map.has_with_attribute
                               d NewVariable t.ioattrs)
                      ) inouts' in
      let inouts' = Dep_set.to_list inouts' in
      let inouts' = if (List.length inouts') < 1 then [] else [Inout inouts'] in
      let depend = List.append ins' inouts' in
      let depend = if (List.length depend) < 1 then [] else [Depend depend] in
      if (Task.attributed t WaitForSome) then
        begin
          if ins' <> [] || inouts' <> [] then
            begin
              let first = List.hd t.current in
              let first = trm_add_pragma (Taskwait depend) first in
              let first = if sync <> [] then
                            trm_add_pragma (Taskwait sync) first
                          else first in
              first :: (List.tl t.current)
            end
          else t.current
        end
      else
        begin
          let shared = [Default Shared_m] in
          let firstprivate = Dep_set.fold (fun d acc ->
                                 match d with
                                 | Dep_var v -> v :: acc
                                 | Dep_trm (t, v) -> v :: acc
                                 | _ -> acc) firstprivate [] in
          let firstprivate =
            if !Apac_flags.cutoff_depth then
              (new_var (get_apac_variable ApacDepthLocal)) :: firstprivate
            else firstprivate
          in
          let firstprivate = if (List.length firstprivate) > 0 then
                               [FirstPrivate firstprivate]
                             else [] in
          let clauses = shared @ depend in
          let clauses = clauses @ firstprivate in
          let clauses =
            match (
              !Apac_flags.profile && (Option.is_some t.cost),
              !Apac_flags.cutoff_count, !Apac_flags.cutoff_depth
            ) with
            | (true, true, true) ->
               let cd = get_cutoff_count_and_depth () in
               let et = get_cutoff_execution_time (Option.get t.cost) in
               clauses @ [If (trm_and cd et)]
            | (true, true, false) ->
               let c = get_cutoff_count () in
               let et = get_cutoff_execution_time (Option.get t.cost) in
               clauses @ [If (trm_and c et)]
            | (true, false, true) ->
               let d = get_cutoff_depth () in
               let et = get_cutoff_execution_time (Option.get t.cost) in
               clauses @ [If (trm_and d et)]
            | (true, false, false) ->
               clauses @ [If (get_cutoff_execution_time (Option.get t.cost))]
            | (false, true, true) ->
               clauses @ [If (get_cutoff_count_and_depth ())]
            | (false, true, false) ->
               clauses @ [If (get_cutoff_count ())]
            | (false, false, true) ->
               clauses @ [If (get_cutoff_depth ())]
            | (false, false, false) -> clauses
          in
          let pragma = Task clauses in
          let instr =
            if !Apac_flags.cutoff_depth then
              (depth_update ()) :: t.current
            else t.current
          in
          let instr =
            if !Apac_flags.cutoff_count then
              instr @ [(count_update true)]
            else instr
          in
          let instr =
            if not (!Apac_flags.cutoff_count || !Apac_flags.cutoff_depth) &&
                 (List.length instr < 2) then
              List.hd instr
            else trm_seq_nomarks instr
          in
          let instr = trm_add_pragma pragma instr in
          let instr =
            if sync <> [] then
              trm_add_pragma (Taskwait sync) instr
            else instr
          in
          if !Apac_flags.cutoff_count then
            [(count_update false); instr]
          else [instr]
        end
    end
  else t.current

(** [insert_tasks_on p t]: see [insert_tasks_on]. *)
let insert_tasks_on (p : path) (t : trm) : trm =
  (** Find the parent function [f]. *)
  let f = match (Apac_miscellaneous.find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_taskify.insert_tasks_on: unable to find parent \
                          function. Task group outside of a function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find Apac_records.functions f in
  (** Translate the task candidate graph representation [r.graph] of [f] to a
      parallel abstract syntax tree. *)
  let ast = TaskGraphTraverse.to_ast codegen_openmp r.graph in
  let ast = Mlist.of_list ast in
  let result = trm_seq ~annot:t.annot ~ctx:t.ctx ast in
  (** Dump the resulting abstract syntax tree, if requested. *)
  if !Apac_flags.verbose then
    begin
      let msg = Printf.sprintf "Parallel abstract syntax tree of `%s'"
                  (var_to_string f) in
      Debug_transfo.trm msg result
    end;
  (** Return the resulting abstract syntax tree. *)
  result

(** [insert_tasks tg]: expects the target [tg] to point at a function body. It
    then translates its task candidate graph representation into a parallel
    abstract syntax tree. *)
let insert_tasks (tg : target) : unit =
  Target.apply (fun t p -> Path.apply_on_path (insert_tasks_on p) t p) tg

(** [cutoff_count_and_depth tg]: expects the target [tg] to point at a
    definition of a taskification candidate function. If the task candidate
    graph of the function features no taskifiable task, i.e. a function
    definition with the [!Apac_macros.candidate_mark]. It then extends its body
    sequence with the definitions of function-local variables for controlling
    task granularity according to the number of submitted tasks and the
    parallelism depth depending on the value of the corresponding flags
    [!Apac_flags.cutoff_count] and [!Apac_flags.cutoff_depth]. The pass also
    adds to the abstract syntax tree of the input program the [#include]
    directives and definitions of global variables involved in these granularity
    control mechanisms.

    For example, let us consider the following C source code and let us suppose
    that both [!Apac_flags.cutoff_count] and [!Apac_flags.cutoff_depth] are set
    to [true].

    {[
    void f(int * tab) { tab[0] += 42; }

    void p(int & v) { int a = 15; int b = a + 2; int c = a + b + v++; }

    /*@__apac_candidate*/ void c(int * tab, int size) {
    /*@__apac_candidate_body*/ {
        f(tab);
        for(int i = 0; i < size; i++) {
          p(tab[i]);
        }
      } /*@__apac_candidate_body*/
    } /*@__apac_candidate*/
    ]}

    Here, the sequence wrapping the body of the function [c] carries the
    [!Apac_macros.candidate_body_mark]. In the first place, the pass adds to the
    beginning of this sequence the definitions of [ApacCountOk],
    [ApacDepthLocal] and [ApacDepth] (see enumeration
    [!type:Apac_macros.apac_variable]).

    {[
    void f(int * tab) { tab[0] += 42; }

    void p(int & v) { int a = 15; int b = a + 2; int c = a + b + v++; }

    /*@__apac_candidate*/ void c(int * tab, int size) {
      int __apac_count_ok =
        __apac_count_infinite || __apac_count < __apac_count_max;
      int __apac_depth_local = __apac_depth;
      int __apac_depth_ok =
        __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    /*@__apac_candidate_body*/ {
        f(tab);
        for(int i = 0; i < size; i++) {
          p(tab[i]);
        }
      } /*@__apac_candidate_body*/
    } /*@__apac_candidate*/
    ]}

    Then, the pass extends the abstract syntax tree of the input program with
    the global definitions of [ApacCountInfinite], [ApacDepthInfinite],
    [ApacCountMax], [ApacDepthMax], [ApacCount] and [ApacDepth] (see enumeration
    [!type:Apac_macros.apac_variable]). Finally, it ensures the presence of
    [#include] directives for the headers [omp.h] and [stdlib.h] the granularity
    control mechanism relies on.

    {[
    #include <omp.h>
    #include <stdlib.h>

    const static int __apac_count_infinite =
      getenv("APAC_TASK_COUNT_INFINITE") ? 1 : 0;

    const static int __apac_depth_infinite =
      getenv("APAC_TASK_DEPTH_INFINITE") ? 1 : 0;
    
    const static int __apac_count_max =
      getenv("APAC_TASK_COUNT_MAX") ?
        atoi(getenv("APAC_TASK_COUNT_MAX")) : omp_get_max_threads() * 10;

    const static int __apac_depth_max =
      getenv("APAC_TASK_DEPTH_MAX") ?
        atoi(getenv("APAC_TASK_DEPTH_MAX")) : 5;

    int __apac_count = 0;

    int __apac_depth = 0;

    #pragma omp threadprivate(__apac_depth)

    void f(int * tab) { tab[0] += 42; }

    void p(int & v) { int a = 15; int b = a + 2; int c = a + b + v++; }

    /*@__apac_candidate*/ void c(int * tab, int size) {
      int __apac_count_ok =
        __apac_count_infinite || __apac_count < __apac_count_max;
      int __apac_depth_local = __apac_depth;
      int __apac_depth_ok =
        __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
    /*@__apac_candidate_body*/ {
        f(tab);
        for(int i = 0; i < size; i++) {
          p(tab[i]);
        }
      } /*@__apac_candidate_body*/
    } /*@__apac_candidate*/
    ]}

    Note that we insert the statements updating and referring to the variables
    we introduce within the [!insert_tasks] pass responsible for the generation
    of parallel source code with OpenMP task-based programming pragmas. The
    [cutoff_count_and_depth] pass is responsible only for declaring,
    initializing and configuring the granularity control variables. *)
let cutoff_count_and_depth (tg : target) : unit =
  let open Apac_macros in
  let open Apac_records in
  (** Initialize a list of sequential implementations to include in the output
      source code. *)
  let sequentials : (trm * target) list ref = ref [] in
  (** Introduce the function-local granularity control variables. *)
  Target.apply (fun t p ->
      Path.apply_on_path (fun t ->
          (** Deconstruct the definition term [t] of the function [f] into its
              return type [ret_ty], the list of its arguments [args] and its
              [body] sequence. *)
          let error = "Apac_parallelization.cutoff_count_and_depth: expected a \
                       target to a function definition." in
          let (f, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
          (** Deconstruct the [body] sequence into an [!module:Mlist] of
              statement terms [s]. *)
          let error = "Apac_parallelization.cutoff_count_and_depth: expected a \
                       body sequence." in
          let s = trm_inv ~error trm_seq_inv body in
          (** [i] is going to represent the sequence of statements we have to
              prepend [s] with. *)
          let i =
            (** If [!Apac_flags.cutoff_count] is [true], we build the definition
                of [!ApacCountOk] (see [!type:Apac_macros.apac_variable]) and
                add it to [i]. *)
            if !Apac_flags.cutoff_count then
              [code
                 (Instr
                    ("int " ^ (get_apac_variable ApacCountOk) ^
                       " = " ^ (get_apac_variable ApacCountInfinite) ^
                         " || " ^ (get_apac_variable ApacCount) ^
                           " < " ^ (get_apac_variable ApacCountMax)))]
            else []
          in
          let i =
            (** If [!Apac_flags.cutoff_depth] is [true], we build the definition
                of [!ApacDepthLocal] as well as the definition of [!ApacDepthOk]
                (see [!type:Apac_macros.apac_variable]) add them to [i]. *)
            if !Apac_flags.cutoff_depth then
              i @ [
                code
                  (Instr
                     ("int " ^ (get_apac_variable ApacDepthLocal) ^
                        " = " ^ (get_apac_variable ApacDepth)));
                code
                  (Instr
                     ("int " ^ (get_apac_variable ApacDepthOk) ^
                        " = " ^ (get_apac_variable ApacDepthInfinite) ^
                          " || " ^ (get_apac_variable ApacDepthLocal) ^
                            " < " ^ (get_apac_variable ApacDepthMax))) 
              ]
            else i
          in
          (** The cut-off mechanism relying on the parallelism depth switches to
              the sequential implementation of [f] when the depth reaches the
              maximum. For this to be possible, *)
          if !Apac_flags.cutoff_depth then
            (** we look for the function variable [v] of the sequential
                implementation of [f] in its function record [r] in
                [!Apac_records.functions]. [exists] indicates whether the input
                source code already contains the sequential implementation of
                [f]. *)
            let r = Var_Hashtbl.find functions f in
            let v, exists =
              match r.sequential with
              | GenerateSequential v -> (v, false)
              | ExistingSequential v -> (v, true)
            in
            (** We then prepare a call [sc] to the sequential version of [f]
                passing it the arguments [args] of [f] as [args'], a list of
                terms. [args] from the definition of [f] is a list of typed
                variables. *)
            let args' = List.map (fun (arg, _) -> trm_var arg) args in
            let sc = trm_apps (trm_var v) args' in
            let sc =
              if not (Typ.is_type_unit ret_ty) then
                trm_ret (Some sc)
              else sc
            in
            (** We transform [sc] into a sequence which *)
            let sc = trm_seq (Mlist.of_list [sc]) in
            (** will constitute the else-branch of an if-conditional [c]
                deciding whether to continue executing the parallel [f] or its
                sequential implementation based on the current parallelism
                depth. *)
            let c = trm_if
                      (code
                         (Expr (get_apac_variable ApacDepthOk))) body sc in
            (** We can now build a new [body] of [f] containing [i] and [c]. *)
            let body = trm_seq (Mlist.of_list (i @ [c])) in
            (** Afterwards, we can update the definition of [f]. *)
            let f = trm_let_fun ~ctx:t.ctx ~annot:t.annot f ret_ty args body in
            (** Finally, we add a sequential implementation of [f] if necessary,
                i.e. if [!exists] is [false]. *)
            if not exists then
              let sequential =
                (** To this end, we deconstruct the original definition term
                    [r.ast] of [f] into its return type [ort], the list of its
                    arguments [oa] and its body sequence [ob]. *)
                let error = "Apac_parallelization.cutoff_count_and_depth: \
                             expected a function definition." in
                let _, ort, oa, ob = trm_inv ~error trm_let_fun_inv r.ast in
                (** We then re-build this function definition again, but we
                    rename it to [v]. *)
                trm_let_fun v ort oa ob
              in
              sequentials := (sequential, (tBefore :: (target_of_path p))) ::
                               !sequentials;
              f
            else 
              (** Otherwise, we simply return the new [f]. *)
              f
          else
            (** When [!Apac_flags.cutoff_depth] is [false], we simply insert [i]
                at the beginning of [s], *)
            let s = Mlist.insert_sublist_at 0 i s in
            (** re-build [s] into an updated [body] and *)
            let body = trm_seq ~ctx:body.ctx ~annot:body.annot s in
            (** return a new definition of [f]. *)
            trm_let_fun ~ctx:t.ctx ~annot:t.annot f ret_ty args body
        ) t p) tg;
  (** Include the sequential implementations in the output source code. *)
  List.iter (fun (sequential, target) ->
      Sequence_basic.insert sequential target
    ) !sequentials;
  (** Introduce the global granularity control variables. *)
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the sequence term [t] representing the root of the
          abstract syntax tree of the input program into an [!module:Mlist] of
          statement terms [s]. *)
      let error = "Apac_parallelization.cutoff_count_and_depth: expected a \
                   target to a statement sequence." in
      let s = trm_inv ~error trm_seq_inv t in
      (** [i] is going to represent the sequence of statements we have to
          prepend [s] with. *)
      let i =
        (** If [!Apac_flags.cutoff_count] is [true], we build the definitions of
            [!ApacCountInfinite], [!ApacCountMax] and [!ApacCount] (see
            [!type:Apac_macros.apac_variable]), then we add them to [i]. *)
        if !Apac_flags.cutoff_count then
          [
            code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacCountInfinite) ^
                      " = getenv(\"" ^ Apac_macros.count_infinite ^
                        "\") ? 1 : 0"));
            code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacCountMax) ^
                      " = getenv(\"" ^ Apac_macros.count_max ^
                        "\") ? atoi(getenv(\"" ^ Apac_macros.count_max ^
                          "\")) : omp_get_max_threads() * " ^
                            (string_of_int
                               !Apac_flags.count_max_thread_factor)));
            code (Instr ("int " ^ (get_apac_variable ApacCount) ^ " = 0"))
          ]
        else []
      in
      let i = 
        (** If [!Apac_flags.cutoff_depth] is [true], we build the definition of
            [!ApacDepthInfinite], [!ApacDepthMax] and the thread-private
            [!ApacDepth] (see [!type:Apac_macros.apac_variable]), then we add
            them to [i]. *)
        if !Apac_flags.cutoff_depth then
          i @ [
            code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacDepthInfinite) ^
                      " = getenv(\"" ^ Apac_macros.depth_infinite ^
                        "\") ? 1 : 0"));
            code
              (Instr
                 ("const static int " ^ (get_apac_variable ApacDepthMax) ^
                    " = getenv(\"" ^ Apac_macros.depth_max ^
                      "\") ? atoi(getenv(\"" ^ Apac_macros.depth_max ^
                        "\")) : " ^
                          (string_of_int
                             !Apac_flags.depth_max_default)));
            code (Instr ("int " ^ (get_apac_variable ApacDepth) ^ " = 0"));
            code (Stmt ("#pragma omp threadprivate(" ^
                          (get_apac_variable ApacDepth) ^ ")"))
          ]
        else i
      in
      (** We can now insert [i] at the beginning of [s] and *)
      let s = Mlist.insert_sublist_at 0 i s in
      (** re-build [s]. *)
      trm_seq ~ctx:t.ctx ~annot:t.annot s
    ) [];
  (** Ensure the [#include] directives for [stdlib.h] and [omp.h]. *)
  Trace.ensure_header "#include <stdlib.h>";
  Trace.ensure_header "#include <omp.h>"

(** [cutoff_execution_time ()]: adds to the abstract syntax tree of the input
    program the definition of a global variable allowing for task granularity
    control relying on execution time models as well as the definition of a
    power computing function if at least one of the latter resorts to in its
    formula (see [!Apac_records.put_pow]).

    For example, let us consider the following C source code.

    {[
    void f() { }
    ]}

    Applying the pass on the above results in

    {[
    const double __apac_cutoff =
      getenv("APAC_EXECUTION_TIME_CUTOFF") ?
        atof(getenv("APAC_EXECUTION_TIME_CUTOFF")) : 2.22100e-6;

    template <class T> T apac_fpow(int exp, const T& base) {
      T result = T(1);
      T pow = base;
      int i = exp;
      while (i) {
        if (i & 1) {
          result *= pow;
        }
        pow *= pow;
        i /= 2;
      }
      return result;
    }
    
    void f() { }
    ]}

    Note that we the task submission conditions referring to the cut-off
    variable we introduce within the [!parallelize] pass responsible for the
    generation of parallel source code with OpenMP task-based programming
    pragmas. The [cutoff_execution_time] pass is responsible only for declaring
    and initializing the granularity control variable as well as the power
    function, if necessary. *)
let cutoff_execution_time () : unit =
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the sequence term [t] representing the root of the
          abstract syntax tree of the input program into an [!module:Mlist] of
          statement terms [s]. *)
      let error = "Apac_parallelization.cutoff_execution_time: expected a \
                   target to a sequence." in
      let s = trm_inv ~error trm_seq_inv t in
      (** Build the definition term [co] of the global variable [ApacCutOff]
          (see type [!type:apac_variable]) while [init]ializing it to the value
          of [!Apac_macros.execution_time_cutoff], if set, or to the default
          value [!Apac_macros.execution_time_min] otherwise. *)
      let init =
        code (Expr
                ("getenv(\"" ^ Apac_macros.execution_time_cutoff ^
                   "\") ? atof(getenv(\"" ^
                     Apac_macros.execution_time_cutoff ^
                       "\")) : " ^ Apac_macros.execution_time_min)) in
      let co = new_var (Apac_macros.get_apac_variable ApacCutOff) in
      let co = trm_let_immut (co, Typ.typ_double ()) init in
      (** Include the definition [!Apac_macros.pow] of the function
          [!Apac_macros.model_pow] to compute powers, if there is at least one
          formula inovlving a power expression. *)
      let definitions =
        if !Apac_records.put_pow then
          [co; code (Stmt Apac_macros.pow)]
        else [co]
      in
      (** Insert the [definitions] at the beginning of [s] and *)
      let s = Mlist.insert_sublist_at 0 definitions s in
      (** re-build [s]. *)
      trm_seq ~ctx:t.ctx ~annot:t.annot s
    ) []

let secure_globals (tg : target) : unit =
  let check (t : trm) : bool =
    trm_fold (fun acc t ->
        let chk = match (trm_var_inv t) with
          | Some v -> Var_map.mem v !Apac_records.globals
          | None -> false in
        acc || chk
      ) false t
  in
  let rec aux (t : trm) : trm * bool =
    match t.desc with
    | Trm_seq stmts ->
       let stmts = Mlist.to_list stmts in
       let stmts = List.map (fun s -> aux s) stmts in
       let rec merge = fun s ->
         match s with
         | (t1, b1) :: (t2, b2) :: tt ->
            if b1 && b2 &&
                 not (trm_has_any_pragmas t1) && not (trm_has_any_pragmas t2)
            then merge ((Syntax.trm_seq_no_brace [t1; t2], true) :: tt)
            else (t1, b1) :: (merge ((t2, b2) :: tt))
         | _ -> s
       in
       let stmts = merge stmts in
       let stmts =
         List.map (fun (t, b) ->
             if b then
               let task =
                 trm_has_pragma (fun p ->
                     match p with Task _ -> true | _ -> false
                   ) t in
               if task then
                 let pragmas = trm_get_pragmas t in
                 let t = trm_rem_pragmas t in
                 let t = trm_append_pragma (Critical (None, None)) t in
                 let t = trm_seq_nomarks [t] in
                 trm_add_pragmas pragmas t
               else
                 let t =
                   if (Nobrace.is_nobrace t) then
                     trm_seq_nomarks [t] else t in       
                 trm_append_pragma (Critical (None, None)) t
             else t
           ) stmts in
       (trm_seq_nomarks ~annot:t.annot stmts, false)
    | Trm_for_c (init, cond, inc, body, _) ->
       if (check init) || (check cond) || (check inc) then (t, true)
       else
         let body, _ = aux body in
         (trm_for_c ~annot:t.annot init cond inc body, false)
    | Trm_for (range, body, _) ->
       let _, init, _, cond, step, _ = range in
       let step = match step with Step st -> check st | _ -> false in
       if (check init) || (check cond) || step then (t, true)
       else
         let body, _ = aux body in
         (trm_for ~annot:t.annot range body, false)
    | Trm_if (cond, yes, no) ->
       if (check cond) then (t, true)
       else
         let yes, _ = aux yes in
         let no, _ = aux no in
         (trm_if ~annot:t.annot cond yes no, false)
    | Trm_while (cond, body) ->
       if (check cond) then (t, true)
       else
         let body, _ = aux body in
         (trm_while ~annot:t.annot cond body, false)
    | Trm_do_while (body, cond) ->
       if (check cond) then (t, true)
       else
         let body, _ = aux body in
         (trm_do_while ~annot:t.annot body cond, false)
    | Trm_switch (cond, cases) ->
       if (check cond) then (t, true)
       else
         let cases =
           List.map (fun (ls, b) ->
               let b, _ = aux b in
               (ls, b)
             ) cases in
         (trm_switch ~annot:t.annot cond cases, false)
    | _ -> (t, check t)
  in
  Nobrace_transfo.remove_after (fun _ ->
      Target.apply_at_target_paths (fun t ->
          Apac_records.globals :=
            Var_map.filter (fun _ (_, v) -> v) !Apac_records.globals;
          let t, _ = aux t in t
        ) tg)
