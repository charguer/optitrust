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

(** [next_id]: generates unique integer identifiers starting from zero. *)
let next_id = Tools.fresh_generator ()

(** [next_profsection]: generates unique names for profiling sections used by
    the profiler backend. See [emit_profiler_task]. *)
let next_profsection () : string =
  let id = next_id () in
  "apac_profsection" ^ (string_of_int id)

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

let emit_omp_task (t : Task.t) : trms =
  if (Task.attributed t WaitForNone) ||
       ((Task.attributed t ExitPoint) && not (Task.attributed t IsJump)) then
    t.current
  else if (Task.attributed t WaitForAll) || (Task.attributed t IsJump) then
    begin
      let pragma = Taskwait [] in
      let first = List.hd t.current in
      let first = trm_add_pragma pragma first in
      first :: (List.tl t.current)
    end
  else
    begin
      let sync = Dep_set.filter (fun d ->
                     Dep_map.has_with_attribute d Accessor t.ioattrs
                   ) t.ins in
      let sync = Dep_set.to_list sync in
      let sync = if (List.length sync) < 1 then [] else [Depend [In sync]] in
      let (firstprivate, ins') =
        if (Task.attributed t WaitForSome) then
          (Dep_set.empty,
           Dep_set.filter (fun d ->
               (Dep_map.has_with_attribute d Condition t.ioattrs) &&
                 not (Dep_map.has_with_attribute d InductionVariable t.ioattrs)
             ) t.ins)
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
      let inouts' = if (Task.attributed t WaitForSome) && t.children <> [] then
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

let emit_profiler_task (t : Task.t) : trms =
  let get_begin (loc : location) : string =
    match loc with
    | None -> "_"
    | Some { loc_start = {pos_line = line; _}; _} -> string_of_int line
  in
  let get_end (loc : location) : string =
    match loc with
    | None -> "_"
    | Some { loc_end = {pos_line = line; _}; _} -> string_of_int line
  in
  if (Task.attributed t ExitPoint) ||
       (Task.attributed t HasJump) ||
         (Task.attributed t WaitForNone) then t.current
  else
    let reads = Dep_set.cardinal t.ins in
    let writes = Dep_set.cardinal t.inouts in
    let first = List.hd t.current in
    let first = get_begin first.loc in
    let last = (List.length t.current) - 1 in
    let last = List.nth t.current last in
    let last = get_end last.loc in
    let profsection = next_profsection () in
    let range = if first <> "_" && last <> "_" then
                  first ^ "-" ^ last
                else profsection in
    let section = "ApacProfilerSection " ^ profsection ^ "(\"" ^
                   range ^ "\", " ^ (string_of_int reads) ^ ", " ^
                        (string_of_int writes) ^ ")" in
    let section = code (Instr section) in
    let ins' = Dep_set.to_list t.ins in
    let ins' = List.map (fun e ->
                   let d = Dep.to_string e in
                   let s =
                     profsection ^ ".addParam(\"" ^ d ^ "\", " ^ d ^ ")" in
                   code (Instr s)
                 ) ins' in
    let inouts' = Dep_set.to_list t.inouts in
    let inouts' = List.map (fun e ->
                      let d = Dep.to_string e in
                      let s =
                        profsection ^ ".addParam(\"" ^ d ^ "\", " ^ d ^ ")" in
                      code (Instr s)
                    ) inouts' in
    let before = code (Instr (profsection ^ ".beforeCall()")) in
    let after = code (Instr (profsection ^ ".afterCall()")) in
    let preamble = (section :: ins') @ inouts' @ [before] in
    let postamble = [after] in
    preamble @ t.current @ postamble

let rec trm_from_task ?(backend : task_backend = OpenMP)
          (t : TaskGraph.V.t) : trms =
  let make (ts : trms) : trm =
    let ts' = Mlist.of_list ts in trm_seq ts'
  in
  (* Get the [Task] element of the current vertex. *)
  let task = TaskGraph.V.label t in
  let current = List.mapi (fun i instr ->
                    begin match instr.desc with
                    | Trm_for_c (init, cond, step, _, _) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                    (trm_from_task ~backend) cg in
                       let body = make body in
                       let body = if backend = OpenMP then
                                    trm_add_mark heapify_breakable_mark body
                                  else
                                    body in
                       trm_for_c ~annot:instr.annot ~ctx:instr.ctx
                         init cond step body
                    | Trm_for (range, _, _) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) cg in
                       let body = make body in
                       let body = if backend = OpenMP then
                                    trm_add_mark heapify_breakable_mark body
                                  else
                                    body in
                       trm_for ~annot:instr.annot ~ctx:instr.ctx
                         range body
                    | Trm_if (cond, _, no) ->
                       let cg = List.nth task.children i in
                       let yes = List.nth cg 0 in
                       let yes = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) yes in
                       let yes = make yes in
                       let yes = if backend = OpenMP then
                                    trm_add_mark heapify_mark yes
                                  else
                                    yes in
                       let no = if (is_trm_unit no) then
                                  trm_unit ()
                                else
                                  let no = List.nth cg 1 in
                                  let no = TaskGraphTraverse.codify
                                             (trm_from_task ~backend) no in
                                  let no = make no in 
                                  if backend = OpenMP then
                                    trm_add_mark heapify_mark no
                                  else
                                    no
                       in
                       trm_if ~annot:instr.annot ~ctx:instr.ctx cond yes no
                    | Trm_while (cond, _) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) cg in
                       let body = make body in
                       let body = if backend = OpenMP then
                                    trm_add_mark heapify_breakable_mark body
                                  else
                                    body in
                       trm_while ~annot:instr.annot ~ctx:instr.ctx cond body
                    | Trm_do_while (_, cond) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) cg in
                       let body = make body in
                       let body = if backend = OpenMP then
                                    trm_add_mark heapify_breakable_mark body
                                  else
                                    body in
                       trm_do_while ~annot:instr.annot ~ctx:instr.ctx body cond
                    | Trm_switch (cond, cases) ->
                       let cg = List.nth task.children i in
                       let cgs = Queue.create () in
                       let _ = List.iter2 (fun (labels, _) block ->
                                   let block' = TaskGraphTraverse.codify
                                                  (trm_from_task ~backend)
                                                  block in
                                   let block' = make block' in
                                   let block' = if backend = OpenMP then
                                                  trm_add_mark
                                                    heapify_breakable_mark
                                                    block'
                                                else
                                                  block' in
                                   Queue.push (labels, block') cgs) cases cg in
                       let cases' = List.of_seq (Queue.to_seq cgs) in
                       trm_switch ~annot:instr.annot ~ctx:instr.ctx cond cases'
                    | Trm_seq _ when backend = OpenMP ->
                       trm_add_mark heapify_mark instr
                    | _ -> instr
                    end) task.current in
  task.current <- current;
  match backend with
  | OpenMP -> emit_omp_task task
  | ApacProfiler -> emit_profiler_task task
