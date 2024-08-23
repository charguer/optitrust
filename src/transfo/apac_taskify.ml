open Ast
open Trm
open Mark
open Path
open Target
open Tools
open Apac_records
open Apac_miscellaneous
open Apac_const
open Apac_dep
open Apac_tasks
open Apac_backend

(** [find_candidates_minimum_funcalls_on ~min p t]: see
    [find_candidates_minimum_funcalls]. *)
let find_candidates_minimum_funcalls_on ?(min : int = 2)
      (p : path) (t : trm) : unit =
  (** Initialize function calls counter. *)
  let counter = ref 0 in
  let rec count (t : trm) : unit =
    match t.desc with
    (** If [t] is a call to a function with a known definition, increase
        [counter] and recurse deeper in the AST. *)
    | Trm_apps ({ desc = Trm_var (_, f)}, _)
         when Var_Hashtbl.mem const_records f ->
       incr counter; trm_iter count t
    (** Otherwise, just recurse deeper in the AST. *)
    | _ -> trm_iter count t
  in
  (** Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_taskify.find_candidates_minimum_funcalls_on: \
                          unable to find parent function. Task group outside \
                          of a function?" in
  (** Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (** Retrieve the root node of the corresponding task graph. *)
  let r = match const_record.task_graph with
    | Some (g) -> TaskGraphOper.root g
    | None -> fail t.loc "Apac_taskify.find_candidates_minimum_funcalls_on: \
                          Missing task graph. Did you taskify?" in
  (** Retrieve the root node's task. *)
  let r = TaskGraph.V.label r in
  (** Count function calls in the underlying AST. *)
  count (List.hd r.current);
  (** If the AST contains at least [min] function calls, mark the function as
      taskification candidate. *)
  if !counter >= min then Var_Hashtbl.add const_candidates f ()

(** [find_candidates_minimum_funcalls ~min tg]: expects the target [tg] to point
    at a function definition. It makes the functions a taskification candidate
    if its body contains at least [min] function calls. *)
let find_candidates_minimum_funcalls ?(min : int = 2) (tg : target) : unit =
  Target.iter (fun t p ->
      find_candidates_minimum_funcalls_on ~min p (get_trm_at_path p t)) tg

(** [taskify_callers ()]: After the selection of candidate functions for the
    taskification, we need to taskify also the functions calling the latter.
    However, only the function calls to the taskification candidates shall be
    transformed into parallelizable tasks. *)
let rec taskify_callers () : unit =
  let is_candidate_caller = ref false in
  let rec calls (c : unit Var_Hashtbl.t) (t : trm) : unit =
    match t.desc with
    | Trm_apps ({ desc = Trm_var (_, f)}, _) when (Var_Hashtbl.mem c f) ->
       is_candidate_caller := true
    | _ -> trm_iter (calls c) t
  in
  let insertions =
    Var_Hashtbl.fold (fun f cr ins ->
        (** We shall update only the callers which has not been qualified as
            taskification candidates yet. *)
        if not (Var_Hashtbl.mem const_candidates f) then
          (** Tells whether the current caller calls one of the taskification
              candidates. *)
          let is_caller = ref false in
          (** Retrieve the task graph of [f]. *)
          let g = match cr.task_graph with
            | Some g' -> g'
            | None -> fail None "Apac_taskify.taskify_callers: Missing task \
                                 graph. Did you taskify?" in
          (** Update its task graph as follows. *)
          TaskGraph.iter_vertex (fun v ->
              (** Get the task [t] from the current task graph vertex [v].*)
              let t : Task.t = TaskGraph.V.label v in
              (** The [body] term of the function [f] corresponding to the
                  current task [t] is the first element in the list of AST terms
                  associated with [t]. *)
              let body = List.hd t.current in
              calls const_candidates body;
              if !is_candidate_caller then
                begin
                  (** If [t] is a function call to one of the taskification
                      candidates, make this call an un-mergeable task with the
                      attributes from the task attribute set [tas]. *)
                  (** Also, the current caller becomes a taskification
                      candidate. *)
                  Var_Hashtbl.add const_candidates f ();
                  is_caller := true;
                  is_candidate_caller := false;
                  t.attrs <- TaskAttr_set.add Singleton t.attrs;
                  t.attrs <- TaskAttr_set.add Taskifiable t.attrs
                end) g;
          (** If the current caller becomes a taskification candidate, it means
              that we make a new insertion into [const_candidates]. *)
          if !is_caller then ins + 1 else ins
        else ins) Apac_records.const_records 0
  in
  (** If the current call to this routine did insert a new element into
      [const_candidates], we have to call it again. *)
  if insertions > 0 then taskify_callers ()

(** [restore_on t]: see [restore]. *)
let restore_on (t : trm) : trm =
  (** Deconstruct the target function definition term. *)
  let error =
    "Apac_taskify.restore_on: expected a target to a function definition" in
  let (f, _, _, _) = trm_inv ~error trm_let_fun_inv t in
  (** If the target function [f] is not a taskification candidate, restore its
      original AST from [Apac_records.const_records]. *)
  if not (Var_Hashtbl.mem const_candidates f) then
    let _ = Printf.printf " > > > > > NOT Candidate: %s\n" (var_to_string f) in
    let cr = Var_Hashtbl.find const_records f in
    cr.ast_backup
  else t

(** [restore tg]: expects the target [tg] to point at a function definition. If
    the function is not a taskification candidate, the transformation shall
    restore the function to its original term. *)
let restore (tg : target) : unit =
  Target.apply_at_target_paths restore_on tg

(* [task_group_on ?mark_group ~master t]: see [task_group] *)
let task_group_on ?(mark_group = false) ~(master : bool) (t : trm) : trm =
  (* Apply [Apac_core.task_group_mark] to the target sequence, if requested. *)
  let t' = if mark_group then
             trm_add_mark Apac_macros.task_group_mark t
           else t in
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

(** [parallel_task_group ?mark_group ?placeholder tg]: expects target [tg] to
   point at a function definition.

    The first step of the transformation consists in replacing return statements
    by gotos. At the beginning of the process, the function's body is wrapped
    into a sequence to which a mark is assigned.  See
    [Apac_prologue.use_goto_for_return] for more details.

    In the second step, we put the marked sequence into an OpenMP task group.
    See [Apac_basic.task_group] for more details.

    If [mark_group] is [true], [task_group] will add the
    [Apac_macros.task_group_mark] to the task group sequence. This way, we can
    target the task group sequences later when inserting tasks into the code.
    Note that the aforementioned mark is not the same thing. That mark is unique
    and serves to identify the target function's body. It only lives within this
    transformation function. We have to use this extra unique mark here,
    otherwise the fourth step could target more than one AST term, which is not
    desirable.

    If [placeholder] is [true], we will create no actual OpenMP task group.
    Instead, the function's body will be simply wrapped into a sequence marked
    with [Apac_macros.task_group_mark]. The intended usage of this option is in
    the case of task profiler activation.

    For the explanation of [master], see [task_group]. *)
let parallel_task_group
      ?(mark_group = false) ?(placeholder = false)
      ?(master = false) : Transfo.t =
  Target.iter (fun t p ->
    (* 1) Create a mark. *)
    let mark = Mark.next() in
    (* 2) Wrap the target function's body into a marked sequence and replace
       return statements by gotos. *)
    Apac_prologue.use_goto_for_return ~mark (target_of_path p);
    (* 3) Get the name of the target function through the deconstruction of the
       corresponding AST term. *)
    let error =
      "Apac_taskify.parallel_task_group: expected a target to a function \
     definition" in
    let (qvar, _, _, _) = trm_inv ~error trm_let_fun_inv (
      Path.get_trm_at_path p t
    ) in
    (* 4) Transform the marked instruction sequence corresponding to the target
       function's body into an OpenMP task group if [placeholder] is [false].

       Note that if the target function is the 'main' function, we want the
       task group to be executed only by one thread, the master thread. *)
    if not placeholder then
      let master = master || (var_has_name qvar !Apac_macros.apac_main) in
      task_group ~mark_group ~master [cMark mark]
    else
      Marks.add Apac_macros.task_group_mark [cMark mark];
    (* 5) Remove the mark. *)
    Marks.remove mark [cMark mark];
  )

(** [trm_look_for_dependencies t]: searches the term [t] for data accesses. It
    returns two lists. The first list holds the access terms where each term is
    paired with an access attribute. The second list contains all the variables
    involved in the data accesses. *)
let trm_discover_dependencies (locals : symbols)
      (t : trm) : (Dep_set.t * Dep_set.t * ioattrs_map * TaskAttr_set.t)  =
  (** [trm_look_for_dependencies.aux ins inouts attrs filter nested fc attr t]:
      builds [ins], a stack of input (read-only) data dependencies in [t],
      [inouts], a stack of input-output (read-write) data dependencies in [t],
      [attrs], a set stack of dependency-dependency attribute pairs indicating
      which of the dependencies have the [Subscripted] or the [Accessor]
      attribute.

      Note that we build stacks by side-effect instead of returning a list.
      This is due to the usage of [trm_iter] for visiting [t]. [trm_iter] allows
      us to call [aux] on each term it visits but the latter has to have a
      [unit] return type.

      When [nested] is [true], the function does not push a new item to stack,
      it simply continues to explore the AST. This happens, for example, in the
      case of a nested get operation such as [***ptr].

      When [fc] is [true], it means that [t] is part of a function call.

      Finally, [attr] allows for passing access attributes between recursive
      calls to [aux], e.g. in the case of nested data accesses (see the
      [access_attr] type for more details). *)
  let rec aux (ins : dep Stack.t) (inouts : dep Stack.t)
            (attrs : (dep * DepAttr_set.t) Stack.t) (filter : Var_set.t)
            (derefs : int) (fc : bool) (attr : DepAttr.t)
            (t : trm) : bool * bool =
    let error = Printf.sprintf "Apac_taskify.trm_look_for_dependencies.aux: \
                                '%s' is not a valid OpenMP depends expression"
                  (AstC_to_c.ast_to_string t) in
    let ebadattr = Printf.sprintf "Apac_taskify.trm_look_for_dependencies.aux: \
                                   dependency attribute '%s' can not be used \
                                   in this context"
                    (DepAttr.to_string attr) in
    (** We iteratively explore [t] and look for: *)
    match t.desc with
    (** - direct variable accesses ('t'), *)
    | Trm_var (vk, v) when not (Var_set.mem v filter) ->
       if not (String.starts_with ~prefix:"sizeof(" v.name) then
         let (degree, exists) = Var_Hashtbl.find_or_default locals v 0 in
         if degree < 1 then
           begin
             let d = Dep_var v in
             match attr with
             | Accessor ->
                Stack.push d ins;
                Stack.push (d, (DepAttr_set.singleton Accessor)) attrs
             | ArgIn -> Stack.push d ins
             | ArgInOut -> Stack.push d inouts
             | _ -> fail t.loc ebadattr
           end
         else
           begin
             let degree' = if fc then degree else derefs in
             let d = Dep.of_degree t v degree' in
             if vk <> Var_immutable then
               begin
                 let t' = trm_get t in
                 let d' = Dep.of_degree t' v degree' in
                 List.iter2 (fun pk pv ->
                     mutables := Dep_map.add pk pv !mutables
                   ) d d';
               end;
             match attr with
             | Accessor ->
                List.iter (fun e ->
                    Stack.push e ins;
                    Stack.push (e, (DepAttr_set.singleton Accessor)) attrs
                  ) d
             | ArgIn -> List.iter (fun e -> Stack.push e ins) d
             | ArgInOut ->
                List.iteri (fun i e ->
                    if ((i > derefs) && fc) || ((not fc) && (i >= derefs)) then
                      Stack.push e inouts
                    else
                      Stack.push e ins) (List.rev d)
             | _ -> fail t.loc ebadattr
           end;
         (exists, false)
       else (true, true)
    (** - get operations ('*t', '**t', ...), *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, _) ->
       let t' = trm_simplify_addressof_and_get t in
       if t <> t' then
         aux ins inouts attrs filter 0 fc attr t'
       else
         begin
           match (trm_resolve_dereferenced_with_degree t) with
           | Some (t', d) ->
              let d' = if attr = ArgInOut && not fc then d else d - 1 in
              aux ins inouts attrs filter d' fc attr t'
           | None -> fail t.loc error
         end
    (** - address operations ('&t'), *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address))}, [t']) ->
       let t'' = trm_simplify_addressof_and_get t in
       if t <> t'' then
         aux ins inouts attrs filter 0 fc attr t''
       else if (trm_is_array_or_direct_access t') then
         aux ins inouts attrs filter 0 fc attr t'
       else
         fail t.loc error
    (** - array accesses ('t\[i\]'), *)
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _) ->
       let (base, accesses) = get_nested_accesses t in
       let base =
         match base.desc with
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [vt]) ->
            vt
         | Trm_var (_, v) -> base
         | _ -> fail base.loc error
       in
       let cd = List.length accesses in
       let exists =
         begin
           match base.desc with
           | Trm_var (_, v) when not (Var_set.mem v filter) ->
              let (td, ex) = Var_Hashtbl.find_or_default locals v 0 in
              let d = Dep.of_array t v in
              let d = if fc && (cd + derefs) < td then
                        (Dep.of_trm t v 1) :: d
                      else d
              in
              begin
                match attr with
                | Accessor ->
                   List.iter (fun e ->
                       Stack.push e ins;
                       Stack.push (e, (DepAttr_set.singleton Accessor)) attrs
                     ) d
                | ArgIn -> List.iter (fun e -> Stack.push e ins) d
                | ArgInOut ->
                   let f = List.hd d in
                   let o = List.tl d in
                   Stack.push f inouts;
                   List.iter (fun e -> Stack.push e ins) o
                | _ -> fail t.loc ebadattr
              end;
              let d' = List.rev d in
              let d' = List.tl d' in
              List.iter (fun rd ->
                  Stack.push (rd, (DepAttr_set.singleton Subscripted)) attrs
                ) d';
              ex
           | _ -> fail base.loc error
         end;
       in
       List.fold_left (fun (e, f) a ->
           match a with
           | Array_access_get t''
             | Array_access_addr t'' ->
              let (e', f') =
                (aux ins inouts attrs filter 0 false Accessor t'') in
              (e && e', f || f')
           | _ -> (e, f)
         ) (exists, false) accesses
    (** - unary increment and decrement operations ('t++', '--t'), *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) when
           (is_prefix_unary op || is_postfix_unary op) ->
       let t'' =
         match t'.desc with
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [vt]) ->
            vt
         | Trm_var (_, v) -> t'
         | _ -> fail t.loc error
       in
       aux ins inouts attrs filter 0 fc ArgInOut t''
    (** - function calls ('f(args)'). *)
    | Trm_apps ({ desc = Trm_var (_ , v); _ }, args) ->
       if Var_Hashtbl.mem const_records v then
         let const_record : const_fun = Var_Hashtbl.find const_records v in
         let exists = ref true in
         List.iteri (
             fun pos arg ->
             if (Int_map.mem pos const_record.const_args) then
               begin
                 let const = Int_map.find pos const_record.const_args in
                 let aa : DepAttr.t =
                   if const.is_const then ArgIn else ArgInOut in
                 let (exists', _) =
                   aux ins inouts attrs filter 0 true aa arg in
                 exists := !exists && exists'
               end
             else
               begin
                 let error =
                   Printf.sprintf
                     "Apac_taskify.trm_look_for_dependencies.aux: the \
                      constification record of '%s' has no argument \
                      constification record for the argument on position \
                      '%d'."
                     (var_to_string v)
                     pos
                 in
                 fail None error
               end
           ) args;
         (!exists, true)
       else
         begin
           List.iter (fun arg ->
               let _ = aux ins inouts attrs filter 0 true ArgInOut arg in ()
             ) args;
           (false, true)
         end
    (** - set operations ('a = 1', 'b = ptr', '*c = 42', ...), *)
    | Trm_apps _ when is_set_operation t ->
       let error' = "Apac_taskify.trm_look_for_dependencies.aux: expected set \
                     operation." in
       let (lval, rval) = trm_inv ~error:error' set_inv t in
       begin
         match (trm_resolve_binop_lval_and_get_with_deref lval) with
         | Some (lv, _) ->
            let (le, lf) =
              aux ins inouts attrs filter 0 false ArgInOut lval in
            let (re, rf) =
              aux ins inouts attrs filter 0 false ArgIn rval in
            (le && re, lf || rf)
         | None -> fail t.loc error
       end
    (** - single variable declarations or defitinitions ('int a',
          'int * b = ptr'), *)
    | Trm_let (vk, (v, ty), init, _) ->
       let degree = typ_get_degree ty in
       let degree = if vk = Var_immutable then degree else degree - 1 in
       (* let d = Dep.of_trm (trm_var ~kind:vk v) v degree in *)
       Stack.push (Dep_var v) inouts;
       Var_Hashtbl.add locals v degree;
       aux ins inouts attrs filter 0 false ArgIn init
    (** - multiple variable declarations or definitions ('int a = 1, b'). *)
    | Trm_let_mult (vk, tvs, inits) ->
       let (vs, _) = List.split tvs in
       let filter = Var_set.of_list vs in
       List.fold_left2 (fun (exists, isfun) (v, ty) init ->
           let degree = typ_get_degree ty in
           let degree = if vk = Var_immutable then degree else degree - 1 in
           (* let d = Dep.of_trm (trm_var ~kind:vk v) v degree in *)
           Stack.push (Dep_var v) inouts;
           Var_Hashtbl.add locals v degree;
           let (exists', isfun') =
             aux ins inouts attrs filter 0 false ArgIn init in
           (exists && exists', isfun || isfun')
         ) (true, false) tvs inits
    (** In the case of any other term, we explore the child terms. *)
    | _ ->
       let exists = ref true in
       let isfun = ref false in
       trm_iter (fun t' ->
           let (exists', isfun') =
             aux ins inouts attrs filter 0 false attr t' in
           exists := !exists && exists';
           isfun := !isfun || isfun'
         ) t;
       (!exists, !isfun)
  in
  (* In the main part of the function, we begin by creating empty stacks to
     contain the discovered in and in-out dependencies as well as an
     dependency-dependecy attribute stack. *)
  let ins : dep Stack.t = Stack.create () in
  let inouts : dep Stack.t = Stack.create () in
  let attrs : (dep * DepAttr_set.t) Stack.t = Stack.create () in
  (* Then, we launch the discovery process using the auxiliary function. *)
  let (_, _) =
    aux ins inouts attrs (Var_set.empty) 0 false ArgIn t in
  (* Finally, we gather the results from the stacks and return them in lists. *)
  let ins' = Dep_set.of_stack ins in
  let inouts' = Dep_set.of_stack inouts in
  let attrs' = Dep_map.of_stack attrs in
  let tas = TaskAttr_set.empty in
  (*let tas = if (not exists) then TaskAttr_set.add WaitForAll tas else tas in
  let tas = if (not isfun) && exists then
              TaskAttr_set.add WaitForSome tas
            else tas in*)
  (ins', inouts', attrs', tas)

(* [taskify_on p t]: see [taskify]. *)
let taskify_on (p : path) (t : trm) : unit =
  (* Auxiliary function to transform a portion of the existing AST into a local
     fill_task_graphed AST (see [atrm]). *)
  let rec fill (s : symbols) (t : trm) (g : TaskGraph.t) : Task.t =
    match t.desc with
    | Trm_seq sequence ->
       (** Keep a copy of the local scope of variables as a set. We need this
           because we do not want any variables potentially defined in child
           scopes (added to [s] within [trm_discover_dependencies]) to end up in
           the dependency sets of the parent scope, which is the current
           scope. *)
       let scope = var_set_of_var_hashtbl s in
       (** Convert the marked list of statements of the [sequence] into a simple
           list of statements. *)
       let instrs = Mlist.to_list sequence in
       (** Transform the statements of the [sequence] into task candidates
           within the task candidate graph [g]. *)
       let tasks = List.map (fun instr -> fill s instr g) instrs in
       (** If the [sequence] features both a task candidate consisting of an
           assignment to [Apac_macros.result_variable] as well as a task
           candidate consisting of a 'goto' jump to [Apac_macros.goto_label] we
           introduce during the the 'return' replacement transformation
           [Apac_prologue.use_goto_for_return], merge them and prevent the
           resulting task candidate from merging with others (see the
           [Singleton] attribute). This makes it easier later to protect these
           statements with a global synchronization barrier within the barrier
           placement transformation [Apac_epilogue.place_barriers]). *)
       let rec merge = fun ts ->
         match ts with
         | t1 :: t2 :: tn ->
            if (Task.attributed t1 IsJump) && (Task.attributed t2 IsJump) then
              let tm = Task.merge t1 t2 in
              tm.attrs <- TaskAttr_set.add Singleton tm.attrs;
              tm :: (merge tn)
            else t1 :: (merge (t2 :: tn))
         | _ -> ts
       in
       let tasks = merge tasks in
       (** The sets of dependencies and the map of dependencies to sets of
           dependency attributes of the [sequence] correspond to the unions of
           the sets of dependencies and the maps of dependencies to sets of
           dependency attributes of all the task candidates. *)
       let (ins, inouts, ioattrs) =
         List.fold_left (
             fun (ins', inouts', ioattrs') (task : Task.t) ->
             (Dep_set.union ins' task.ins,
              Dep_set.union inouts' task.inouts,
              Dep_map.union2 ioattrs' task.ioattrs))
           (Dep_set.empty, Dep_set.empty, Dep_map.empty) tasks in
       (** Initialize a new schedule generator for the task candidates. *)
       let schedule : (unit -> int) = Tools.fresh_generator_from_zero () in
       (** Generate the task candidate to represent the [sequence] and add it to
           the task candidate graph [g]. It gets the logical schedule [0] and
           receives the [Singleton] attribute. Indeed, just like selection and
           iteration statements (see below), compound statements are not
           mergeable with other task candidates. *)
       let this =
         Task.create (schedule ()) t (TaskAttr_set.singleton Singleton) scope
           ins inouts ioattrs [] in
       (** The corresponding vertex in [g] shall become the root vertex of the
           graph (see further below). *)
       let this' = TaskGraph.V.create this in
       TaskGraph.add_vertex g this';
       Printf.printf "\n\n\nLIST OF TASKS\n\n\n";
       (** Add the task candidates representing the statements of the [sequence]
           into the task candidate graph [g] while assigning them a logical
           schedule. *)
       let tasks = List.map (
                       fun task ->
                       Printf.printf "Task: %s\n" (Task.to_string task);
                       task.schedule <- schedule ();
                       let v = TaskGraph.V.create task in
                       TaskGraph.add_vertex g v; v
                     ) tasks in
       (** Translate dependencies between task candidates into edges in the task
           candidate graph [g]. To do this, we check each couple of task
           candidates in [tasks]. *)
       let nb_tasks = List.length tasks in
       for i = 0 to (nb_tasks - 1) do
         let vertex_i = List.nth tasks i in
         let task_i = TaskGraph.V.label vertex_i in
         for j = (i + 1) to (nb_tasks - 1) do
           let vertex_j = List.nth tasks j in
           let task_j = TaskGraph.V.label vertex_j in
           (** A task candidate [j] depend on a task candidate [i] (with [i <>
               j]) either when the Bernstein's condition verify (see
               [Task.depending]) or when one of the task candidates requests a
               global synchronization barrier (see the [WaitForAll] task
               candidate attribute). *)
           let j_depends_on_i = Task.depending task_i task_j in
           let j_depends_on_i = j_depends_on_i ||
                                  Task.attributed task_j ExitPoint ||
                                    Task.attributed task_j IsJump ||
                                      Task.attributed task_i WaitForAll ||
                                        Task.attributed task_j WaitForAll
           in
           if j_depends_on_i then
             begin
               Printf.printf "'%s' ---> '%s'\n"
                 (Task.to_excerpt task_i) (Task.to_excerpt task_j);
               TaskGraph.add_edge g vertex_i vertex_j
             end
         done
       done;
       (** To make [this] the root vertex, add an edge from [this] to any other
           vertex without predecessors. *)
       for i = 0 to (nb_tasks - 1) do
         let vertex = List.nth tasks i in
         let degree = TaskGraph.in_degree g vertex in
         if degree < 1 then
           begin
             TaskGraph.add_edge g this' vertex
           end
       done;
       (** Return the final vertex representing the [sequence] in the task
           candidate graph [g]. *)
       this      
    | Trm_for_c (init, cond, inc, instr, _) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the dependency sets of the parent scope, which is the current
          scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Launch dependency discovery in the initialization term as well as *)
       let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s init in
       (* in the conditional statement representing the upper loop bound. *)
       let (ins', inouts', ioattrs', tas') =
         trm_discover_dependencies s cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton Condition) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton Condition) ioattrs' in
       (* Gather the discovered dependencies and attributes. *)
       let (ins, inouts, ioattrs, tas) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs',
          TaskAttr_set.union2 tas tas') in
       (* Launch dependency discovery in the increment term. *)
       let (ins', inouts', ioattrs', tas') =
         trm_discover_dependencies s inc in
       (* Add the [InductionVariable] attribute to the input and input-output
          dependencies discovered in the increment term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       (* Gather the discovered dependencies and attributes. *)
       let (ins, inouts, ioattrs, tas) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs',
          TaskAttr_set.union2 tas tas') in
       (* Create a sub-graph for the body sequence, i.e. [instr], of the
          for-loop. *)
       let c = TaskGraph.create() in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let ct = fill s instr c in
       (* Propagate the [InductionVariable] attribute to the input and
          input-output dependencies discovered in the body sequence of the
          for-loop whenever they feature the dependencies from [ins'] and
          [inouts'], i.e. the sets of dependencies previously discovered in the
          increment term of the for-loop. *)
       TaskGraphOper.propagate_dependency_attribute
         (DepAttr_set.singleton InductionVariable) ins' c;
       TaskGraphOper.propagate_dependency_attribute
         (DepAttr_set.singleton InductionVariable) inouts' c;
       (** The [Condition], the [Accessor] and the [Subscripted] dependency
           attributes are not valid outside of their initial scope. *)
       ct.ioattrs <- Dep_map.remove_attribute Condition ct.ioattrs;
       ct.ioattrs <- Dep_map.remove_attribute Accessor ct.ioattrs;
       ct.ioattrs <- Dep_map.remove_attribute Subscripted ct.ioattrs;
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [for] graph node, i.e. [ins] and [inouts],
          by the means of a union operation. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ct.ins,
          Dep_set.union inouts ct.inouts,
          Dep_map.union2 ioattrs ct.ioattrs) in
       (* A for-loop node should not be merged with other potential tasks. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Create the task corresponding to the current for-node using all the
          elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[c]]
    | Trm_for (range, instr, _) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the dependency sets of the parent scope, which is the current
          scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Explode the [range] specifier to allow for dependency discovery. *)
       let (index, init, _, cond, step, _) = range in
       (* Launch dependency discovery in the initialization term as well as *)
       let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s init in
       (** Add the iterator [index] to the scope of the current loop. *)
       Var_Hashtbl.add s index 0;
       (* in the conditional statement representing the upper loop bound. *)
       let (ins', inouts', ioattrs', tas') =
         trm_discover_dependencies s cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton Condition) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton Condition) ioattrs' in
       (* Gather the discovered dependencies and attributes. *)
       let (ins, inouts, ioattrs, tas) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs',
          TaskAttr_set.union2 tas tas') in
       (* Check whether [step] is formed of a term. In other words, check
          whether it is not simply a unary increment or decrement, but something
          like [i += a * 2]. In this case, *)
       let (ins', inouts', ioattrs', tas') = match step with
         (* we have to look for dependencies in this term. *)
         | Step st -> trm_discover_dependencies s st
         (* Otherwise, we have to add an input-output dependency on the
            induction variable [index]. *)
         | _ ->
            let div = Dep_var index in
            (ins, Dep_set.add div inouts, ioattrs, tas)
       in
       (* Add the [InductionVariable] attribute to the input and input-output
          dependencies discovered in the increment term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       (* Gather the discovered dependencies, if any. *)
       let (ins, inouts, ioattrs, tas) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs',
          TaskAttr_set.union2 tas tas') in
       (* Create a sub-graph for the body sequence, i.e. [instr], of the
          for-loop. *)
       let c = TaskGraph.create() in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let ct = fill s instr c in
       (* Propagate the [InductionVariable] attribute to the input and
          input-output dependencies discovered in the body sequence of the
          for-loop whenever they feature the dependencies from [ins'] and
          [inouts'], i.e. the sets of dependencies previously discovered in the
          increment term of the for-loop. *)
       TaskGraphOper.propagate_dependency_attribute
         (DepAttr_set.singleton InductionVariable) ins' c;
       TaskGraphOper.propagate_dependency_attribute
         (DepAttr_set.singleton InductionVariable) inouts' c;
       (** The [Condition], the [Accessor] and the [Subscripted] dependency
           attributes are not valid outside of their initial scope. *)
       ct.ioattrs <- Dep_map.remove_attribute Condition ct.ioattrs;
       ct.ioattrs <- Dep_map.remove_attribute Accessor ct.ioattrs;
       ct.ioattrs <- Dep_map.remove_attribute Subscripted ct.ioattrs;
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [for] graph node, i.e. [ins] and [inouts],
          by the means of a union operation. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ct.ins,
          Dep_set.union inouts ct.inouts,
          Dep_map.union2 ioattrs ct.ioattrs) in
       (* A for-loop node should not be merged with other potential tasks. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Create the task corresponding to the current for-node using all the
          elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[c]] 
    | Trm_let _
      | Trm_let_mult _ ->
       (* Look for dependencies in the current variable declaration term and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts, _, tas) = trm_discover_dependencies s t in
       (* Convert the updated local scope to a set. *)
       let scope' = var_set_of_var_hashtbl s in
       (* Variable declarations should never become tasks, but rather
          synchronization barriers, nor be merged with other task graph
          nodes. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Create a barrier corresponding to the current variable declaration
          term. Variable declarations should never appear in tasks. *)
       Task.create (-1) t tas scope' ins inouts Dep_map.empty [[]]
    | Trm_apps _ ->
       (** Is [t] an assignment to [Apac_macros.result_variable] we introduce in
           the 'return' replacement [Apac_prologue.use_goto_for_return]? *)
       let isjump = if (is_set_operation t) then
                      let error = "Apac_taskify.taskify_on.fill: expected set \
                                   operation." in
                      let (lval, _) = trm_inv ~error set_inv t in
                      match trm_resolve_binop_lval_and_get_with_deref lval with
                      | Some (lvar, _) when
                             lvar.v.name = Apac_macros.result_variable -> true
                      | _ -> false
                    else false
       in
       if isjump then
         (** If so, we must make it clear the statement is related to a 'goto'
             jump to [Apac_macros.goto_label], hence the [IsJump] attribute. *)
         let attrs = TaskAttr_set.singleton IsJump in
         Task.create (-1) t attrs Var_set.empty
           Dep_set.empty Dep_set.empty Dep_map.empty []
       else
         (** Look for dependencies and their attributes in the current term and
             initialize the in and inout-dependency sets as well as the map of
             dependency attribute sets. *)
         let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s t in
         (** Convert the local scope to a set. *)
         let scope = var_set_of_var_hashtbl s in
         (** Create the corresponding task candidate using all the elements
             computed above. *)
         Task.create (-1) t tas scope ins inouts ioattrs [[]]
    | Trm_if (cond, yes, no) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies and their attributes in the conditional
          expression of the [if] and initialize the in and in-out dependency
          sets as well as the map of dependency attribute sets. *)
       let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the if-statement. *)
       let ioattrs = Dep_map.bind_set ins
                       (DepAttr_set.singleton Condition) ioattrs in
       let ioattrs = Dep_map.bind_set inouts
                       (DepAttr_set.singleton Condition) ioattrs in
       (* Create sub-graphs for the [then] and the [else] branches. *)
       let gy = TaskGraph.create () in
       let gn = TaskGraph.create () in
       (* Taskify the branches while filling the correspoding sub-graphs. *)
       let ty = fill s yes gy in
       (* If there is no [else] branch, create an empty task. *)
       let missing_tn = is_trm_unit no in 
       let tn = if missing_tn then Task.empty () else fill s no gn in
       (** The [Condition], the [Accessor] and the [Subscripted] dependency
           attributes are not valid outside of their initial scope. *)
       ty.ioattrs <- Dep_map.remove_attribute Condition ty.ioattrs;
       ty.ioattrs <- Dep_map.remove_attribute Accessor ty.ioattrs;
       ty.ioattrs <- Dep_map.remove_attribute Subscripted ty.ioattrs;
       tn.ioattrs <- Dep_map.remove_attribute Condition tn.ioattrs;
       tn.ioattrs <- Dep_map.remove_attribute Accessor tn.ioattrs;
       tn.ioattrs <- Dep_map.remove_attribute Subscripted tn.ioattrs;
       (* Include the dependencies and their attributes from the branches into
          the current [if] graph node, i.e. [ins], [inouts] and [ioattrs], by
          the means of union operations. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ty.ins,
          Dep_set.union inouts ty.inouts,
          Dep_map.union2 ioattrs ty.ioattrs) in
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins tn.ins,
          Dep_set.union inouts tn.inouts,
          Dep_map.union2 ioattrs tn.ioattrs) in
       (* An [if] node should not be merged with other potential tasks. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Initialize the list of sub-graphs corresponding to the [then] branch
          and, if present, for the [else] branch too. *)
       let children = if missing_tn then [] else [gn] in
       let children = gy :: children in
       (* Create the task corresponding to the current [if] graph node using all
          the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [children]
    | Trm_while (cond, body) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies and their attributes in the conditional
          expression of the [while] and initialize the in and in-out dependency
          sets as well as the map of dependency attributes. *)
       let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the while-loop. *)
       let ioattrs = Dep_map.bind_set ins
                       (DepAttr_set.singleton Condition) ioattrs in
       let ioattrs = Dep_map.bind_set inouts
                       (DepAttr_set.singleton Condition) ioattrs in
       (* Create a sub-graph for the body sequence of the [while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s body gb in
       (** The [Condition], the [Accessor] and the [Subscripted] dependency
           attributes are not valid outside of their initial scope. *)
       tb.ioattrs <- Dep_map.remove_attribute Condition tb.ioattrs;
       tb.ioattrs <- Dep_map.remove_attribute Accessor tb.ioattrs;
       tb.ioattrs <- Dep_map.remove_attribute Subscripted tb.ioattrs;
       (* Include the dependencies and their attributes from the body sequence
          into the current [while] graph node, i.e. into [ins], [inouts] and
          [ioattrs], by the means of a union operation. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins tb.ins,
          Dep_set.union inouts tb.inouts,
          Dep_map.union2 ioattrs tb.ioattrs) in
       (* A while-loop node should not be merged with other potential tasks. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Create the task corresponding to the current [while] graph node using
          all the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[gb]]
    | Trm_do_while (body, cond) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the do-while
          and initialize the in and in-out dependency sets as well as the map of
          dependency attribute sets. *)
       let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the do-while-loop. *)
       let ioattrs = Dep_map.bind_set ins
                       (DepAttr_set.singleton Condition) ioattrs in
       let ioattrs = Dep_map.bind_set inouts
                       (DepAttr_set.singleton Condition) ioattrs in
       (* Create a sub-graph for the body sequence of the [do-while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s body gb in
       (** The [Condition], the [Accessor] and the [Subscripted] dependency
           attributes are not valid outside of their initial scope. *)
       tb.ioattrs <- Dep_map.remove_attribute Condition tb.ioattrs;
       tb.ioattrs <- Dep_map.remove_attribute Accessor tb.ioattrs;
       tb.ioattrs <- Dep_map.remove_attribute Subscripted tb.ioattrs;
       (* Include the dependencies and their attributes from the body sequence
          into the current [do-while] graph node, i.e. [ins], [inouts] and
          [ioattrs], by the means of a union operation. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins tb.ins,
          Dep_set.union inouts tb.inouts,
          Dep_map.union2 ioattrs tb.ioattrs) in
       (* A do-while-loop node should not be merged with other potential
          tasks. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Create the task corresponding to the current [do-while] graph node
          using all the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[gb]]
    | Trm_switch (cond, cases) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies and their attributes in the conditional
          expression of the [switch] and initialize the in and in-out dependency
          sets as well as the map of dependency attribute sets. *)
       let (ins, inouts, ioattrs, tas) = trm_discover_dependencies s cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the
          switch-statement. *)
       let ioattrs = Dep_map.bind_set ins
                       (DepAttr_set.singleton Condition) ioattrs in
       let ioattrs = Dep_map.bind_set inouts
                       (DepAttr_set.singleton Condition) ioattrs in
       (* We are about to process the blocks associated with the cases of the
          [switch]. To each case we will associate the corresponding [Task] and
          [TaskGraph]. As we can not directly map the elements of [cases] to
          another type of list elements, i.e. pairs of [Task] and [TaskGraph],
          we well keep these pairs in the temporary queue [cases']. *)
       let cases' = Queue.create () in
       (* For each block associated with one of the cases of the [switch],
          we: *)
       List.iter (fun (labels, block) ->
           (* - create a sub-graph for the block sequence, *)
           let gb = TaskGraph.create () in
           (* - taskify the block sequence while filling the
              corresponding graph, *)
           let tb = fill s block gb in
           (* - push both the task and the graph associated
              with the currently processed block sequence into
              the temporary stack. *)
           Queue.push (tb, gb) cases'
         ) cases;
       (* Get the [Task] and [TaskGraph] elements from the temporary stack as a
          pair of lists. *)
       let pairs = List.of_seq (Queue.to_seq cases') in
       let (tbs, gbs) = List.split pairs in
       (* Include the dependencies and their attributes from the block sequences
          into the current [switch] graph node, i.e. [ins], [inouts] and
          [ioattrs], by the means of union operations. *)
       let (ins, inouts, ioattrs) =
         List.fold_left (fun (ins', inouts', ioattrs') (tb : Task.t) ->
             (** The [Condition], the [Accessor] and the [Subscripted]
                 dependency attributes are not valid outside of their initial
                 scope. *)
             tb.ioattrs <- Dep_map.remove_attribute Condition tb.ioattrs;
             tb.ioattrs <- Dep_map.remove_attribute Accessor tb.ioattrs;
             tb.ioattrs <- Dep_map.remove_attribute Subscripted tb.ioattrs;
             (Dep_set.union ins' tb.ins,
              Dep_set.union inouts' tb.inouts,
              Dep_map.union2 ioattrs' tb.ioattrs))
           (ins, inouts, ioattrs) tbs in
       (* A switch node should not be merged with other potential tasks. *)
       let tas = TaskAttr_set.add Singleton tas in
       (* Create the task corresponding to the current [switch] graph node
          using all the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [gbs]
    | Trm_delete (_, target) ->
       (* Look for dependencies in the target term of the [delete]. [delete] is
          a destructive operation, we need to consider all of the dependencies
          as in-out dependencies, of course. *)
       let (ins, inouts, _, tas) = trm_discover_dependencies s target in
       let inouts = Dep_set.union ins inouts in
       (* Convert the local scope to a set *)
       let scope = var_set_of_var_hashtbl s in
       (* Transform this task into a synchronization barrier. See
          [Apac_tasks.TaskAttr]. *)
       let tas = TaskAttr_set.union2
                   tas (TaskAttr_set.singleton WaitForSome) in
       (* in order to be able to use it when creating the task corresponding to
          the current [delete] graph node. *)
       Task.create (-1) t tas scope Dep_set.empty inouts Dep_map.empty [[]]
    | Trm_goto target ->
       (** If the target label of the 'goto' is not the [Apac_core.goto_label]
           we use within the return statement replacement transformation
           [Apac_basic.use_goto_for_return], fail. We do not allow for other
           'goto' statements than [Apac_core.goto_label]. *)
       if target <> Apac_macros.goto_label then
         fail t.loc "Apac_taskify.taskify_on.fill: illegal 'goto' statement"
       else
         (** If [target] is [Apac_core.goto_label], we can transform it into a
             task candidate carrying the [IsJump] attribute to indicate the
             presence of the 'goto' statement. *)
         let attrs = TaskAttr_set.singleton IsJump in
         Task.create (-1) t attrs Var_set.empty
           Dep_set.empty Dep_set.empty Dep_map.empty [[]]
    | Trm_val v ->
       (** Retrieve the first label attribute of the current term, if any. *)
       let l = trm_get_labels t in
       let l = if (List.length l) > 0 then List.nth l 0 else "" in
       (** Check whether the label is [Apac_core.goto_label] we use within the
           return statement replacement transformation
           [Apac_basic.use_goto_for_return]. *)
       begin match v with
       (** If so, we can transform it into a task candidate carrying the
           [Singleton] and the [ExitPoint] attributes. See [TaskAttr.t] for more
           details on the meaning of task candidate attributes. *)
       | Val_lit (Lit_unit) when l = Apac_macros.goto_label ->
          let attrs = TaskAttr_set.singleton Singleton in
          let attrs = TaskAttr_set.add ExitPoint attrs in
          Task.create (-1) t attrs Var_set.empty
            Dep_set.empty Dep_set.empty Dep_map.empty [[]]
       (** Otherwise, fail. We do not allow for other types of values in
           first-level instructions. *)
       | _ ->
          let it = AstC_to_c.ast_to_string t in
          let error =
            Printf.sprintf
              "Apac_taskify.taskify_on.fill: illegal value term '%s'" it in
          fail t.loc error
       end
    | Trm_omp_routine r ->
       (* Convert the local scope to a set. *)
       let scope = var_set_of_var_hashtbl s in
       (* Calls to OpenMP routines must not become parallelizable tasks! *)
       let attrs = TaskAttr_set.singleton WaitForAll in
       (* When it comes to OpenMP routine calls, we consider two different
          situations: *)
       begin match r with
       (* 1) On the one hand, there are routines taking a variable as an
          argument. In this case, we have to perform dependency discovery. *)
       | Set_default_device v
         | Init_lock v 
         | Init_nest_lock v 
         | Destroy_lock v 
         | Destroy_nest_lock v
         | Set_lock v 
         | Set_nest_lock v 
         | Unset_lock v 
         | Unset_nest_lock v 
         | Test_lock v 
         | Test_nest_lock v ->
          (* Look for dependencies in the routine argument [v]. The above
             routines modify the variable they take as an argument. Therefore,
             we need to consider all of the dependencies as in-out
             dependencies. *)
          let (ins, inouts, _, _) = trm_discover_dependencies s (trm_var v) in
          let inouts = Dep_set.union ins inouts in
          (* Create the task corresponding to the current OpenMP routine call
             graph node. *)
          Task.create (-1) t attrs scope Dep_set.empty inouts Dep_map.empty [[]]
       (* 2) On the other hand, all the other routines do not involve any
          variables and thus do not require dependency discovery. *)
       | _ ->
          (* Create the task corresponding to the current OpenMP routine call
             graph node. *)
          Task.create
            (-1) t attrs scope Dep_set.empty Dep_set.empty Dep_map.empty [[]]
       end
    | _ ->
       let error = Printf.sprintf
                     "Apac_core.taskify_on.fill: '%s' should not appear in a \
                      task group" (trm_desc_to_string t.desc) in
       fail t.loc error
  in
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.taskify_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = TaskGraph.create () in
  let _ = fill const_record.variables t g in
  let g' = TaskGraphOper.recursive_transitive_reduction g in
  const_record.task_graph <- Some (g');
  Printf.printf "Task graph of << %s >> follows:\n" (var_to_string f);
  TaskGraphPrinter.print g';
  let dot = (cwd ()) ^ "/apac_task_graph_" ^ f.name ^ ".dot" in
  export_task_graph g' dot;
  dot_to_pdf dot
    
let taskify (tg : target) : unit =
  Target.iter (fun t p -> taskify_on p (get_trm_at_path p t)) tg

(** [merge_on p t]: see [merge]. *)
let merge_on (p : path) (t : trm) : unit =
  (** [merge_on.seq g start]: find the longest sequence of mergeable vertices
      beginning with the [start] vertex in the task candidate graph [g]. *)
  let rec seq (g : TaskGraph.t) (start : TaskGraph.V.t) :
            TaskGraph.V.t list =
    (** Retrieve the successors of the first vertex, i.e. the [start] vertex, in
        the future sequence. *)
    let next = TaskGraph.succ g start in
    (** If the [start] vertex does not have exactly one successor, it can not
        represent the beginning of a sequence of vertices to merge. Indeed,
        multiple successors indicate independent task candidates and there are
        no vertices to merge if there are no successors. In these cases, we
        simply return a single-vertex sequence with the [start] vertex. *)
    if (List.length next) <> 1 then [start]
    else
      let next = List.hd next in
      (** Otherwise, we check whether the successor *)
      let next' = TaskGraph.V.label next in
      (** can be merged, i.e. does not carry the [Singleton] attribute or both
          [start] and its successor represent global synchronization barriers
          (see the [WaitForAll] attribute), and *)
      if (not (Task.attributed next' Singleton) ||
            (Task.attributed (TaskGraph.V.label start) WaitForAll) &&
              (Task.attributed next' WaitForAll)) &&
           (** whether it has no other predecessors than [start]. Indeed,
               merging such a node with another one would break the original
               lexicographic order of the input program. *)
           (TaskGraph.in_degree g next) < 2 then
        (** If the successor of [start] meets the above conditions, we can
            include it into the sequence. *)
        start :: (seq g next)
      (** Otherwise, we simply return a single-vertex sequence with the [start]
          vertex. *)
      else [start]
  in
  (** [merge_on.one g]: traverse the task candidate graph [g] in search for
      sequences of mergeable vertices, perform the merges, if any, and update
      connecting edges. *)
  let rec one (g : TaskGraph.t) : unit =
    (** Retrieve all the vertices of [g] in a list. *)
    let vs = TaskGraph.fold_vertex (fun v acc -> v::acc) g [] in
    (** Get the number of vertices in [g]. *)
    let nb = TaskGraph.nb_vertex g in
    (** Loop over the list of vertices of [g] and *)
    for i = 0 to (nb - 1) do
      (** for each of them: *)
      let vi = List.nth vs i in
      (** - retrieve the label, *)
      let ti = TaskGraph.V.label vi in
      (** - check whether we can merge it with other vertices, i.e. whether it
          does not carry the [Singleton] attribute or whether it carries the
          [WaitForAll] attribute, and whether it is not the root vertex of [g],
          i.e. whether it has at least one predecessor. Indeed, the root vertex
          is a symbolic vertex representing the entire tuple of statements
          behind [g] and therefore, we should not merge it with any other
          vertex. Note that we have to verify the existence of [vi] in [g] too.
          This is due to the fact that merging involves vertex removal from [g].
          However, this removal does not affect the list of vertices [vs] of [g]
          we work on and which we retrieved at the very beginning of the merge
          transformation. *)
      if (not (Task.attributed ti Singleton)
          || (Task.attributed ti WaitForAll)) &&
           (TaskGraph.mem_vertex g vi) &&
             (TaskGraph.in_degree g vi > 0)
      then
        (** If the i-th vertex [vi] represents a valid beginning of a potential
            sequences of mergeable vertices, constitue the largest such
            sequence. *)
        let s : TaskGraph.V.t list = seq g vi in
        (** Get the length of the resulting sequence. *)
        let slen = List.length s in
        (** If there is more than one element, there are vertices to merge. *)
        if slen > 1 then
          begin
            (** [vi] is the first element of [s]. Get the other elements to
                merge with [vi]. *)
            let others = List.tl s in
            (** Get the label of [vi]. *)
            let first = TaskGraph.V.label vi in
            (** Merge the labels of [others] with the label of [vi]. *)
            let task : Task.t = List.fold_left (fun t v ->
                                    let c : Task.t = TaskGraph.V.label v in
                                    Task.merge t c
                                  ) first others in
            (** Update the label elements of [vi] for it to represent the merged
                task candidate. *)
            first.current <- task.current;
            first.attrs <- task.attrs;
            first.ins <- task.ins;
            first.inouts <- task.inouts;
            first.ioattrs <- task.ioattrs;
            first.children <- task.children;
            (** Retrieve the last vertex in the sequence and *)
            let last = List.nth s (slen - 1) in
            (** make its successors the successors of [vi]. *)
            TaskGraph.iter_succ (fun v ->
                TaskGraph.add_edge g vi v) g last;
            (** Remove the merged vertices. *)
            List.iter (fun v -> TaskGraph.remove_vertex g v) others
          end
    done;
    TaskGraph.iter_vertex (fun v ->
        let t : Task.t = TaskGraph.V.label v in
        List.iter (fun l -> List.iter (fun g -> one g) l) t.children) g
  in
  (** Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_taskify.merge_on: unable to find parent \
                          function. Task group outside of a function?" in
  (** Find its constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (** Retrieve the associated task candidate graph. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_taskify.merge_on: Missing task candidate graph. \
                          Did you taskify?" in
  (** Apply the merging transformation on the task candidate graph. *)
  one g;
  Printf.printf "Merged task graph of << %s >> follows:\n" (var_to_string f);
  TaskGraphPrinter.print g;
  (** Save the resulting graph to a DOT, then to a PDF file. *)
  let dot = (cwd ()) ^ "/apac_task_graph_" ^ f.name ^ "_merged.dot" in
  export_task_graph g dot;
  dot_to_pdf dot

let merge (tg : target) : unit =
  Nobrace.enter ();
  Target.iter (fun t p -> merge_on p (get_trm_at_path p t)) tg

(** [detect_tasks_simple_on p t]: see [detect_tasks_simple_on]. *)
let detect_tasks_simple_on (p : path) (t : trm) : unit =
  (** [detect_tasks_simple_on.aux v]: if the vertex [v] features a call to a
      function we know the definition of, attribute it [Taskifiable]. Otherwise,
      if [v] involves nested candidate graphs, process them recursively. *)
  let rec aux (v : TaskGraph.V.t) : unit =
    let t = TaskGraph.V.label v in
    if (List.exists (fun s ->
            match s.desc with
            | Trm_apps ({ desc = Trm_var (_ , f); _ }, _)
                 when Var_Hashtbl.mem const_records f -> true
            | _ -> false) t.current) then
      t.attrs <- TaskAttr_set.add Taskifiable t.attrs;
    (** Process the task candidates in nested task candidate graphs, if any. *)
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter aux go
          ) gl
      ) t.children
  in
  (** Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_taskify.detect_tasks_simple_on: unable to find \
                          parent function. Task group outside of a function?" in
  (** Find its constification record in [Apac_const.const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (** Check whether the function definition has a task candidate graph. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_taskify.detect_tasks_simple_on: Missing task \
                          candidate graph. Did you taskify?" in
  (** Add the [Taskifiable] attribute to every task candidate featuring a call
      to a function we know the definition of. *)
  TaskGraphTraverse.iter aux g;
  (** Save the resulting graph to a DOT, then to a PDF file. *)
  let dot = (cwd ()) ^ "/apac_task_graph_" ^ f.name ^ "_detect.dot" in
  export_task_graph g dot;
  dot_to_pdf dot

let detect_tasks_simple (tg : target) : unit =
  Target.iter (fun t p -> detect_tasks_simple_on p (get_trm_at_path p t)) tg

(* [insert_tasks_on p t]: see [insert_tasks_on]. *)
(* TODO: mettre sur papier des idées de stratégies de transformation de
   graphes. *)
let insert_tasks_on (p : path) (t : trm) : trm =
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.insert_tasks_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_core.merge_on: Missing task graph. Did you \
                          taskify?" in
  let instrs = TaskGraphTraverse.codify (trm_from_task ~backend:OpenMP) g in
  let instrs = Mlist.of_list instrs in
  let result = trm_seq ~annot:t.annot ~ctx:t.ctx instrs in
  let _ = Debug_transfo.trm "output" result in
  result
    
let insert_tasks (tg : target) : unit =
  Target.apply (fun t p -> Path.apply_on_path (insert_tasks_on p) t p) tg

(* [profile_tasks_on p t]: see [profile_tasks_on]. *)
let profile_tasks_on (p : path) (t : trm) : trm =
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.profile_tasks_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_core.profile_tasks_on: Missing task graph. Did \
                          you taskify?" in
  let instrs = TaskGraphTraverse.codify
                 (trm_from_task ~backend:ApacProfiler) g in
  let instrs = Mlist.of_list instrs in
  let result = trm_seq ~annot:t.annot ~ctx:t.ctx instrs in
  result

let profile_tasks (tg : target) : unit =
  Trace.ensure_header Apac_macros.profiler_header;
  Target.apply (fun t p -> Path.apply_on_path (profile_tasks_on p) t p) tg

