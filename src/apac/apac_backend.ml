open Ast
open Trm
open Mark
open Apac_macros
open Apac_dep
open Apac_tasks

(* [task_backend]: enumeration of supported task runtimes. *)
type task_backend =
  | OpenMP (* Emit OpenMP tasks. *)
  | ApacProfiler (* Do not create tasks. Insert calls to profiling functions
                    instead. *)

let emit_omp_task (t : Task.t) : trms =
  if (Task.has_attr t ExitPoint) ||
       (Task.has_attr t HasJump) ||
         (Task.has_attr t WaitForNone) then t.current
  else if (Task.has_attr t WaitForAll) then
    begin
      let pragma = Taskwait [] in
      let first = List.hd t.current in
      let first = trm_add_pragma pragma first in
      first :: (List.tl t.current)
    end
  else
    begin
      let (firstprivate, ins') =
        if (Task.has_attr t WaitForSome) then
          (Dep_set.empty,
           Dep_set.filter (fun d ->
               Dep_map.has_with_attribute d Condition t.ioattrs
             ) t.ins)
        else
          Dep_set.partition (fun d ->
              Dep_map.has_with_attribute d InductionVariable t.ioattrs
            ) t.ins
      in
      let ins' = Dep_set.to_list ins' in
      let ins' = if (List.length ins') < 1 then [] else [In ins'] in
      let inouts' = if (Task.has_attr t WaitForSome) then
                      Dep_set.filter (fun d ->
                          Dep_map.has_with_attribute d Condition t.ioattrs
                        ) t.inouts
                    else t.inouts in
      let inouts' = Dep_set.to_list inouts' in
      let inouts' = if (List.length inouts') < 1 then [] else [Inout inouts'] in
      let depend = List.append ins' inouts' in
      let depend = if (List.length depend) < 1 then [] else [Depend depend] in
      if (Task.has_attr t WaitForSome) then
        begin
          if ins' <> [] || inouts' <> [] then
            begin
              let pragma = Taskwait depend in
              let first = List.hd t.current in
              let first = trm_add_pragma pragma first in
              first :: (List.tl t.current)
            end
          else t.current
        end
      else
        begin
          let shared = [Default Shared_m] in
          let firstprivate = Dep_set.fold (fun d acc ->
                                 let d' = Dep.to_atomic d in
                                 match d' with
                                 | Dep_var v -> v :: acc
                                 | Dep_trm (t, v) -> v :: acc
                                 | _ -> acc) firstprivate [] in
          let firstprivate = if (List.length firstprivate) > 0 then
                               [FirstPrivate firstprivate]
                             else [] in
          let clauses = shared @ depend in
          let clauses = clauses @ firstprivate in
          let pragma = Task clauses in
          let instr = if (List.length t.current) < 2 then
                        List.hd t.current
                      else
                        trm_seq_nomarks t.current in
          [trm_add_pragma pragma instr]
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
  let reads = Dep_set.cardinal t.ins in
  let writes = Dep_set.cardinal t.inouts in
  let first = List.hd t.current in
  let first = get_begin first.loc in
  let last = (List.length t.current) - 1 in
  let last = List.nth t.current last in
  let last = get_end last.loc in
  let section = "ApacProfilerSection profsection(\"" ^
                  first ^ "-" ^ last ^ "\", " ^
                    (string_of_int reads) ^ ", " ^
                      (string_of_int writes) ^ ")" in
  let section = code (Instr section) in
  let ins' = Dep_set.to_list t.ins in
  let ins' = List.map (fun e ->
                 let d = Dep.to_string e in
                 let s = "profsection.addParam(\"" ^ d ^ "\", " ^ d ^ ")" in
                 code (Instr s)
               ) ins' in
  let inouts' = Dep_set.to_list t.inouts in
  let inouts' = List.map (fun e ->
                    let d = Dep.to_string e in
                    let s = "profsection.addParam(\"" ^ d ^ "\", " ^ d ^ ")" in
                    code (Instr s)
                  ) inouts' in
  let before = code (Instr "profsection.beforeCall()") in
  let after = code (Instr "profsection.afterCall()") in
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
  let task = Task.update task current in
  match backend with
  | OpenMP -> emit_omp_task task
  | ApacProfiler -> emit_profiler_task task
