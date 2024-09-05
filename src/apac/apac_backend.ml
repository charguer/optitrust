open Ast
open Trm
open Mark
open Apac_macros
open Apac_dep
open Apac_tasks

(** [task_backend]: enumeration of supported task runtimes. *)
type task_backend =
  | OpenMP (* Emit OpenMP tasks. *)
  | ApacProfiler (* Do not create tasks. Insert calls to profiling functions
                    instead. *)

(** [apac_variable]: enumeration of instrumentation variables that might appear
    in the resulting source code. *)
type apac_variable =
  | ApacCount (* Gives the current task count. *)
  | ApacDepth (* Gives the current task depth. *)
  | ApacDepthLocal (* Task-private copy of [ApacDepth]. *)
  | ApacCountOk (* True if the task count limit was not reached yet. *)
  | ApacDepthOk (* True if the task depth limit was not reached yet. *)
  | ApacCountInfinite (* True when the task count is not limited. *)
  | ApacDepthInfinite (* True when the task depth is not limited. *)
  | ApacCountMax (* Gives the maximum task count. *)
  | ApacDepthMax (* Gives the maximum task depth. *)

(** [get_apac_variable]: generates a string representation of the
    instrumentation variable [v]. *)
let get_apac_variable (v : apac_variable) : string =
  match v with
  | ApacCount -> "__apac_count"
  | ApacDepth -> "__apac_depth"
  | ApacDepthLocal -> "__apac_depth_local"
  | ApacCountOk -> "__apac_count_ok"
  | ApacDepthOk -> "__apac_depth_ok"
  | ApacCountInfinite -> "__apac_count_infinite"
  | ApacDepthInfinite -> "__apac_depth_infinite"
  | ApacCountMax -> "__apac_count_max"
  | ApacDepthMax -> "__apac_depth_max"

(** [count_update ~backend postamble]: generates the portion of the
    instrumentation code allowing to update the task count [ApacCount] variable.
    If [postamble] is [false], it produces code to increment the counter when a
    new task is about to be spawned. In order to produce code to decrement the
    counter at the end of a task, set [postamble] to [true]. Also, by default,
    the target backend is OpenMP. This can be changed through the optional
    [backend] argument. *)
let count_update ?(backend : task_backend = OpenMP) (postamble : bool) : trm =
  (* Retrieve the string representation of the involved instrumentation
     variables. *)
  let count = get_apac_variable ApacCount in
  let ok = get_apac_variable ApacCountOk in
  (* Decide on the update operation to apply. *)
  let op = if postamble then "--" else "++" in
  (* Build the task count update term. *)
  let update = code (Instr (count ^ op)) in
  (* When the target backend is *)
  let update = match backend with
    | OpenMP ->
       (* Prepend it with the OpenMP atomic pragma. *)
       trm_add_pragma (Atomic None) update
    | ApacProfiler ->
       (* Keep it as is. *)
       update
  in
  (* Put the update statement into a sequence. *)
  let update = trm_seq_nomarks [update] in
  (* Convert the string representation of [ok] to a term prior to *)
  let condition = code (Expr ok) in
  (* building the final if-conditional. *)
  trm_if condition update (trm_unit ())

(** [depth_update]: generate the portion of the instrumentation code allowing
    to update the task depth [ApacDepth] variable at the beginning of a newly
    spawned task. *)
let depth_update () : trm =
  (* Retrieve the string representation of the involved instrumentation
     variables. *)
  let count = get_apac_variable ApacDepth in
  let local = get_apac_variable ApacDepthLocal in
  let ok1 = get_apac_variable ApacCountOk in
  let ok2 = get_apac_variable ApacDepthOk in
  (* Build the task depth increment term. *)
  let increment = code (Instr (count ^ " = " ^ local ^ " + 1")) in
  (* Put the increment term into a sequence. *)
  let increment = trm_seq_nomarks [increment] in
  (* Convert the string representations of [ok1] and [ok2] to a boolean
     expression term prior to *)
  let condition = code (Expr (ok1 ^ " || " ^ ok2)) in
  (* building the final if-conditional. *)
  trm_if condition increment (trm_unit ())

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
      let ins' = List.map (fun d ->
                     match (Dep_map.find_opt d !Apac_records.mutables) with
                     | Some d' -> d'
                     | None -> d) ins' in
      let ins' = if (List.length ins') < 1 then [] else [In ins'] in
      let inouts' = if (Task.attributed t WaitForSome) && t.children <> [[]]
                    then
                      Dep_set.filter (fun d ->
                          Dep_map.has_with_attribute d Condition t.ioattrs
                        ) t.inouts
                    else t.inouts in
      let inouts' = Dep_set.to_list inouts' in
      let inouts' = List.map (fun d ->
                        match (Dep_map.find_opt d !Apac_records.mutables) with
                        | Some d' -> d'
                        | None -> d) inouts' in
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
          let depth_local_var = new_var (get_apac_variable ApacDepthLocal) in
          let firstprivate = if !Apac_macros.instrument_code then
                               depth_local_var :: firstprivate
                             else firstprivate in
          let firstprivate = if (List.length firstprivate) > 0 then
                               [FirstPrivate firstprivate]
                             else [] in
          let cutoff = (get_apac_variable ApacCountOk) ^ " || " ^
                         (get_apac_variable ApacDepthOk) in
          let cutoff = [If cutoff] in
          let clauses = shared @ depend in
          let clauses = clauses @ firstprivate in
          let clauses = if !Apac_macros.instrument_code then
                          clauses @ cutoff
                        else clauses in
          let pragma = Task clauses in
          let count_preamble = count_update false in
          let depth_preamble = depth_update () in
          let count_postamble = count_update true in
          let instr = if !Apac_macros.instrument_code then
                        depth_preamble :: t.current
                      else t.current in
          let instr = if !Apac_macros.instrument_code then
                        instr @ [count_postamble]
                      else instr in
          let instr = if not (!Apac_macros.instrument_code) &&
                           (List.length instr < 2) then
                        List.hd instr
                      else trm_seq_nomarks instr in
          let instr = trm_add_pragma pragma instr in
          let instr = if sync <> [] then
                        trm_add_pragma (Taskwait sync) instr
                      else instr in
          if !Apac_macros.instrument_code then [count_preamble; instr]
          else [instr]
        end
    end
  else t.current

let rec trm_from_task_candidate ?(heapification : bool = true)
          (codegen : TaskGraph.V.t -> trms) (candidate : TaskGraph.V.t) : trms =
  (* Get the [Task] element of the current vertex. *)
  let t = TaskGraph.V.label candidate in
  t.current <- List.mapi (fun i instr ->
                   begin match instr.desc with
                   | Trm_for_c (init, cond, step, _, _) ->
                      let cg = List.nth t.children i in
                      let cg = List.hd cg in
                      let body = TaskGraphTraverse.codify
                                   (trm_from_task_candidate
                                      ~heapification backend)
                                   cg in
                      let body = trm_seq (Mlist.of_list body) in
                      let body = if heapification then
                                   trm_add_mark heapify_breakable_mark body
                                 else
                                   body in
                      trm_for_c ~annot:instr.annot ~ctx:instr.ctx
                        init cond step body
                   | Trm_for (range, _, _) ->
                      let cg = List.nth t.children i in
                      let cg = List.hd cg in
                      let body = TaskGraphTraverse.codify
                                   (trm_from_task_candidate
                                      ~heapification backend)
                                   cg in
                      let body = trm_seq (Mlist.of_list body) in
                      let body = if heapification then
                                   trm_add_mark heapify_breakable_mark body
                                 else
                                   body in
                      trm_for ~annot:instr.annot ~ctx:instr.ctx
                        range body
                   | Trm_if (cond, _, no) ->
                      let cg = List.nth t.children i in
                      let yes = List.nth cg 0 in
                      let yes = TaskGraphTraverse.codify
                                  (trm_from_task_candidate
                                     ~heapification backend)
                                  yes in
                      let yes = trm_seq (Mlist.of_list yes) in
                      let yes = if heapification then
                                  trm_add_mark heapify_mark yes
                                else
                                  yes in
                      let no = if (is_trm_unit no) then
                                 trm_unit ()
                               else
                                 let no = List.nth cg 1 in
                                 let no = TaskGraphTraverse.codify
                                            (trm_from_task_candidate
                                               ~heapification backend)
                                            no in
                                 let no = trm_seq (Mlist.of_list no) in 
                                 if heapification then
                                   trm_add_mark heapify_mark no
                                 else
                                   no
                      in
                      trm_if ~annot:instr.annot ~ctx:instr.ctx cond yes no
                   | Trm_while (cond, _) ->
                      let cg = List.nth t.children i in
                      let cg = List.hd cg in
                      let body = TaskGraphTraverse.codify
                                   (trm_from_task_candidate
                                      ~heapification backend)
                                   cg in
                      let body = trm_seq (Mlist.of_list body) in
                      let body = if heapification then
                                   trm_add_mark heapify_breakable_mark body
                                 else
                                   body in
                      trm_while ~annot:instr.annot ~ctx:instr.ctx cond body
                   | Trm_do_while (_, cond) ->
                      let cg = List.nth t.children i in
                      let cg = List.hd cg in
                      let body = TaskGraphTraverse.codify
                                   (trm_from_task_candidate
                                      ~heapification backend)
                                   cg in
                      let body = trm_seq (Mlist.of_list body) in
                      let body = if heapification then
                                   trm_add_mark heapify_breakable_mark body
                                 else
                                   body in
                      trm_do_while ~annot:instr.annot ~ctx:instr.ctx body cond
                   | Trm_switch (cond, cases) ->
                      let cg = List.nth t.children i in
                      let cgs = Queue.create () in
                      let _ = List.iter2 (fun (labels, _) block ->
                                  let block' = TaskGraphTraverse.codify
                                                 (trm_from_task_candidate
                                                    ~heapification backend)
                                                 block in
                                  let block' = trm_seq (Mlist.of_list block') in
                                  let block' = if heapification then
                                                 trm_add_mark
                                                   heapify_breakable_mark
                                                   block'
                                               else
                                                 block' in
                                  Queue.push (labels, block') cgs) cases cg in
                      let cases' = List.of_seq (Queue.to_seq cgs) in
                      trm_switch ~annot:instr.annot ~ctx:instr.ctx cond cases'
                   | Trm_seq _ when heapification ->
                      trm_add_mark heapify_mark instr
                   | _ -> instr
                   end) t.current;
  codegen candidate

(** [profile_tasks tg]: expects the target [tg] to point at a function body. It
    then translates its task candidate graph representation into an abstract
    syntax tree while surrounding eligible task candidates with profiling
    sections. *)
let profile_tasks (tg : target) : unit =
  (** Include the header providing profiling elements. *)
  Trace.ensure_header Apac_macros.profiler_header;
  Target.apply (fun t p ->
      Path.apply_on_path (fun t ->
          (** Find the parent function [f]. *)
          let f = match (find_parent_function p) with
            | Some (v) -> v
            | None -> fail t.loc "Apac_backend.profile_tasks: unable to find \
                                  parent function. Taskification candidate \
                                  body outside of a task candidate?" in
          (** Find its function record [r] in [!Apac_records.functions]. *)
          let r = Var_Hashtbl.find functions f in
          (** Initialize a stack [sections] for storing the definitions of
              future profiling sections. *)
          let sections = Stack.create () in
          (** Translate the task candidate graph representation [r.graph] of [f]
              to a abstract syntax tree using the profiler back-end. *)
          let ast = TaskGraphTraverse.codify
                      (trm_from_task_candidate
                         ~heapification:false codegen_profiler)
                      r.graph in
          let sections = List.of_seq (Stack.to_seq sections) in
          let ast = Mlist.of_list (sections @ ast) in
          let result = trm_seq ~annot:t.annot ~ctx:t.ctx ast in
          (** Dump the resulting abstract syntax tree, if requested. *)
          if !Apac_macros.verbose then
            begin
              let msg = Printf.sprintf "Abstract syntax tree of `%s' with profiling \
                                        instructions" (var_to_string f) in
              Debug_transfo.trm msg result
            end;
          (** Return the resulting abstract syntax tree. *)
          result
        ) t p) tg

(** [insert_tasks_on p t]: see [insert_tasks_on]. *)
let insert_tasks_on (p : path) (t : trm) : trm =
  (** Find the parent function [f]. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_taskify.insert_tasks_on: unable to find parent \
                          function. Task group outside of a function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find functions f in
  (** Translate the task candidate graph representation [r.graph] of [f] to a
      parallel abstract syntax tree. *)
  let ast = TaskGraphTraverse.codify (
                trm_from_task_candidate codegen_openmp
              ) r.graph in
  let ast = Mlist.of_list ast in
  let result = trm_seq ~annot:t.annot ~ctx:t.ctx ast in
  (** Dump the resulting abstract syntax tree, if requested. *)
  if !Apac_macros.verbose then
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
