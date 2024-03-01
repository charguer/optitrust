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

(* [parallel_task_group ?mark_group tg]: expects target [tg] to point at a
   function definition.

    The first step of the transformation consists in replacing return statements
    by gotos. At the beginning of the process, the function's body is wrapped
    into a sequence to which a mark is assigned.  See
    [Apac_basic.use_goto_for_return] for more details.

    In the second step, we put the marked sequence into an OpenMP task group.
    See [Apac_basic.task_group] for more details.

    If [mark_group] is [true], [Apac_basic.task_group] will add the
    [Apac_core.task_group_mark] to the task group sequence. This way, we can
    target the task group sequences later when inserting tasks into the
    code. Note that the aforementioned mark is not the same thing. That mark is
    unique and serves to identify the target function's body. It only lives
    within this transformation function. We have to use this extra unique mark
    here, otherwise the fourth step could target more than one AST term, which
    is not desirable. *)
let parallel_task_group ?(mark_group = false) : Transfo.t =
  Target.iter (fun t p ->
    (* 1) Create a mark. *)
    let mark = Mark.next() in
    (* 2) Wrap the target function's body into a marked sequence and replace
       return statements by gotos. *)
    Apac_prologue.use_goto_for_return ~mark (target_of_path p);
    (* 3) Get the name of the target function through the deconstruction of the
       corresponding AST term. *)
    let error =
    "Apac.parallel_task_group: expected a target to a function definition" in
    let (qvar, _, _, _) = trm_inv ~error trm_let_fun_inv (
      Path.get_trm_at_path p t
    ) in
    (* 4) Transform the marked instruction sequence corresponding to the target
       function's body into an OpenMP task group.

       Note that if the target function is the 'main' function, we want the
       task group to be executed only by one thread, the master thread. *)
    task_group ~mark_group ~master:(var_has_name qvar "main") [cMark mark];
    (* 5) Remove the mark. *)
    Marks.remove mark [cMark mark];
  )

(* [trm_look_for_dependencies t]: searches the term [t] for data accesses. It
   returns two lists. The first list holds the access terms where each term is
   paired with an access attribute. The second list contains all the variables
   involved in the data accesses. *)
let trm_discover_dependencies (locals : symbols)
      (t : trm) : (Dep_set.t * Dep_set.t)  =
  (* [trm_look_for_dependencies.aux depends nested attr t] builds [depends], a
     stack of data accesses in [t] and variables involved in the latter. Note
     that we build the stack by side-effect instead of returning a list. This is
     due to the usage of [trm_iter] for visiting [t]. [trm_iter] allows us to
     call [aux] on each term it visits but the latter has to have a [unit]
     return type. When [nested] is [true], the function does not push a new item
     to the stack, it simply continues to explore the AST. This happens, for
     example, in the case of a nested get operation such as [***ptr]. Finally,
     [attr] allows for passing access attributes between recursive calls to
     [aux], e.g. in the case of nested data accesses (see the [access_attr] type
     for more details). *)
  let rec aux (ins : dep Stack.t) (inouts : dep Stack.t)
            (filter : Var_set.t) (nested : bool) (attr : Dep.attr)
            (t : trm) : unit =
    let error = Printf.sprintf "Apac_core.trm_look_for_dependencies.aux: '%s' \
                                is not a valid OpenMP depends expression"
                  (AstC_to_c.ast_to_string t) in
    (* We iteratively explore [t] and look for: *)
    (*let _ = Printf.printf "What term? %s\n" (trm_desc_to_string t.desc) in*)
    match t.desc with
    | Trm_var (_, v) when not nested && not (Var_set.mem v filter) ->
       (*let _ = Printf.printf "Trm_var %s\n" (var_to_string v) in*)
       if String.starts_with ~prefix:"sizeof(" v.name then
         let d = Dep_var (v) in Stack.push d ins
       else
         begin
           match attr with
           | Regular -> let d = Dep_var (v) in Stack.push d ins
           | FunArgIn ->
              let degree = Var_Hashtbl.find locals v in
              let d = Dep.of_var v degree in
              Stack.push d ins
           | FunArgInOut ->
              let degree = Var_Hashtbl.find locals v in
              let d = Dep.of_var v degree in
              Stack.push d inouts
         end
    (* - get operations ([*t'], [**t'], ...), *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [t']) ->
       if not nested then
         begin
           match (trm_resolve_pointer_and_degree t') with
           | Some (v, degree) when not (Var_set.mem v filter) ->
              begin
                match attr with
                | Regular -> let d = Dep_trm (t', v) in Stack.push d ins
                | FunArgIn -> 
                   let degree' = Var_Hashtbl.find locals v in
                   let d = Dep.of_trm t' v (degree' - degree) in
                   Stack.push d ins
                | FunArgInOut ->
                   let degree' = Var_Hashtbl.find locals v in
                   for i = 0 to (degree' + degree) do
                     (* TODO: Pas sûr ! Avant, il y avait Dep.of_var. *)
                     let d = Dep.of_trm t' v i in
                     Stack.push d inouts
                   done
              end
           | Some _ -> ()
           | None -> fail t.loc error
         end;
       trm_iter (aux ins inouts filter true Regular) t'
    (* - address operations ([&t'], ...), *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address))}, [t']) ->
       (*let _ = Printf.printf "address\n" in*)
       if not nested then
         begin
           match (trm_resolve_pointer_and_degree t') with
           | Some (v, _) when not (Var_set.mem v filter) ->
              (*let _ = Printf.printf "address of %s\n" (var_to_string v) in*)
              begin
                match attr with
                | Regular
                  | FunArgIn -> let d = Dep_trm (t, v) in Stack.push d ins
                | FunArgInOut -> let d = Dep_trm (t, v) in Stack.push d inouts
              end
           | Some _ -> ()
           | None -> fail t.loc error
         end;
       trm_iter (aux ins inouts filter true Regular) t'
    (* - array accesses ([t'[i]], [t' -> [i]]), *)
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _)
      | Trm_apps ({desc = Trm_val
                            (Val_prim (Prim_binop Binop_array_get)); _}, _) ->
       let (base, accesses) = get_nested_accesses t in
       begin
         match (trm_resolve_pointer_and_degree base) with
         | Some (v, _) when not nested && not (Var_set.mem v filter) ->
            begin
              match attr with
              | Regular -> let d = Dep_trm (t, v) in Stack.push d ins
              | FunArgIn ->
                 let degree = List.length accesses in
                 let degree' = Var_Hashtbl.find locals v in
                 let d = Dep.of_trm t v (degree' - degree) in
                 Stack.push d ins
              | FunArgInOut ->
                 let degree = List.length accesses in
                 let degree' = Var_Hashtbl.find locals v in
                 let d = Dep.of_trm t v (degree' - degree) in
                 Stack.push d inouts
            end
         | Some _ -> ()
         | None -> fail t.loc error
       end;
       List.iter (
           fun a -> match a with
                    | Array_access_get t''
                      | Array_access_addr t'' ->
                       aux ins inouts filter false Regular t''
                    | _ -> ()
         ) accesses
    (* - unary increment and decrement operations ([t'++], [--t'], ...), *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) when
           (is_prefix_unary op || is_postfix_unary op) ->
       (* let _ = Printf.printf "unop\n" in *)
       begin
         match (trm_resolve_pointer_and_degree t') with
         | Some (v, degree) when not (Var_set.mem v filter) ->
            begin
              match attr with
              | Regular -> let d = Dep_var (v) in Stack.push d inouts
              | FunArgIn
                | FunArgInOut ->
                 let degree = degree + 1 in
                 let degree' = Var_Hashtbl.find locals v in
                 let d = Dep.of_var v (degree' - degree) in
                 Stack.push d inouts
            end
         | Some _ -> ()
         | None -> fail t.loc error
       end;
    (* - function calls ([f(args)], ...). *)
    | Trm_apps ({ desc = Trm_var (_ , v); _ }, args) ->
       (*let _ = Printf.printf "call to %s\n" (var_to_string v) in*)
       if Var_Hashtbl.mem const_records v then
         let const_record : const_fun = Var_Hashtbl.find const_records v in
         List.iteri (
             fun pos arg ->
             if (Int_map.mem pos const_record.const_args) then
               begin
                 let const = Int_map.find pos const_record.const_args in
                 if const.is_const then
                   aux ins inouts filter false FunArgIn arg
                 else
                   aux ins inouts filter false FunArgInOut arg
               end
             else
               begin
                 let error =
                   Printf.sprintf
                     "Apac_core.trm_discover_dependencies: the constification \
                      record of '%s' has no argument constification record for
                      the argument on position '%d'."
                     (var_to_string v)
                     pos
                 in
                 fail None error
               end
           ) args
       else
         List.iter (
             fun arg ->
             trm_iter (aux ins inouts filter false FunArgInOut) arg
           ) args
    (* - set operation ([a = 1], [b = ptr], [*c = 42], ...), *)
    | Trm_apps _ when is_set_operation t ->
       let error' = "Apac_core.trm_look_for_dependencies.aux: expected set \
                     operation." in
       let (lval, rval) = trm_inv ~error:error' set_inv t in
       begin
         match (trm_resolve_binop_lval_and_get_with_deref lval) with
         | Some (lv, _) ->
            let d = Dep_trm (lval, lv.v) in
            Stack.push d inouts;
            trm_iter (aux ins inouts filter false Regular) rval
         | None -> fail t.loc error
       end
    | Trm_let (vk, (v, ty), init, _) ->
       let d = Dep_var (v) in
       let degree = (typ_get_degree ty) - 1 in
       Stack.push d inouts;
       for i = 1 to degree do
         let d' = Dep.of_var v i in
         Stack.push d' inouts
       done;
       Var_Hashtbl.add locals v degree;
       trm_iter (aux ins inouts filter false Regular) init
    | Trm_let_mult (vk, tvs, inits) ->
       let (vs, _) = List.split tvs in
       let filter = Var_set.of_list vs in
       List.iter2 (fun (v, ty) init ->
           let d = Dep_var (v) in
           let degree = (typ_get_degree ty) - 1 in
           Stack.push d inouts;
           for i = 1 to degree do
             let d' = Dep.of_var v i in
             Stack.push d' inouts
           done;
           Var_Hashtbl.add locals v degree;
           trm_iter (aux ins inouts filter false Regular) init
         ) tvs inits
    (* In the case of any other term, we just continue to explore the child
       terms. *)
    | _ -> (*let _ = Debug_transfo.trm ~style:Internal "other" t in*)
       trm_iter (aux ins inouts filter false attr) t
  in
  (* In the main part of the function, we begin by creating empty stacks to
     contain the discovered in and in-out dependencies. *)
  let ins : dep Stack.t = Stack.create () in
  let inouts : dep Stack.t = Stack.create () in
  (* Then, we launch the discovery process using the auxiliary function. *)
  let _ = aux ins inouts (Var_set.empty) false Regular t in
  (* Finally, we gather the results from the stacks and return them in lists. *)
  let ins' = Dep_set.of_stack ins in
  let inouts' = Dep_set.of_stack inouts in
  (ins', inouts')

(* [taskify_on p t]: see [taskify]. *)
let taskify_on (p : path) (t : trm) : unit =
  (* Auxiliary function to transform a portion of the existing AST into a local
     fill_task_graphed AST (see [atrm]). *)
  let rec fill (s : symbols) (t : trm) (g : TaskGraph.t) : Task.t =
    match t.desc with
    | Trm_seq sequence ->
       let scope = var_set_of_var_hashtbl s in
       let instrs = Mlist.to_list sequence in
       let tasks = List.map (fun instr -> fill s instr g) instrs in
       let ins = List.fold_left (
                     fun acc (task : Task.t) -> Dep_set.union acc task.ins
                   ) Dep_set.empty tasks in
       let inouts = List.fold_left (
                        fun acc (task : Task.t) -> Dep_set.union acc task.inouts
                      ) Dep_set.empty tasks in
       let has_jump = List.exists (fun e -> Task.has_attr e HasJump) tasks in
       let attrs = if has_jump then TaskAttr_set.singleton HasJump
                   else TaskAttr_set.empty in
       let this = Task.create t attrs scope ins inouts [] in
       let this' = TaskGraph.V.create this in
       let _ = TaskGraph.add_vertex g this' in
       let tasks = List.map (
                       fun task ->
                       let v = TaskGraph.V.create task in
                       TaskGraph.add_vertex g v; v
                     ) tasks in
       let _ = TaskGraph.add_edge g this' (List.hd tasks) in
       let nb_tasks = List.length tasks in
       for i = 0 to (nb_tasks - 1) do
         let vertex_i = List.nth tasks i in
         let task_i = TaskGraph.V.label vertex_i in
         for j = (i + 1) to (nb_tasks - 1) do
           let vertex_j = List.nth tasks j in
           let task_j = TaskGraph.V.label vertex_j in
           let op1 = Dep_set.inter task_i.inouts task_j.ins in
           let op2 = Dep_set.inter task_i.inouts task_j.inouts in
           let j_depends_on_i =
             not ((Dep_set.is_empty op1) && (Dep_set.is_empty op2)) in
           let j_depends_on_i = j_depends_on_i ||
                                  Task.has_attr task_j ExitPoint in
           if j_depends_on_i then
             begin
               TaskGraph.add_edge g vertex_i vertex_j
             end
           else
             begin
               () (*TaskGraph.add_edge g this' vertex_j*)
             end
         done
       done;
       for i = 0 to (nb_tasks - 1) do
         let vertex = List.nth tasks i in
         let degree = TaskGraph.in_degree g vertex in
         if degree < 1 then
           begin
             TaskGraph.add_edge g this' vertex
           end
       done;
       this      
    | Trm_for_c (init, cond, inc, instr, _) ->
       let scope = var_set_of_var_hashtbl s in
       let (ins, inouts) = trm_discover_dependencies s init in
       let (ins', inouts') = trm_discover_dependencies s cond in
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       let (ins', inouts') = trm_discover_dependencies s inc in
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       let c = TaskGraph.create() in
       let ct = fill s instr c in
       let (ins, inouts) =
         (Dep_set.union ins ct.ins, Dep_set.union inouts ct.inouts) in
       let _ = 
         TaskGraph.iter_vertex (fun vertex ->
             let lab : Task.t = TaskGraph.V.label vertex in
             Printf.printf "subgraph vertex: %s\n" (Task.to_string lab)) c in
       let attrs = if (Task.has_attr ct HasJump) then
                     TaskAttr_set.singleton HasJump
                   else TaskAttr_set.empty in
       Task.create t attrs scope ins inouts [[c]]
    | Trm_for (range, instr, _) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Explode the [range] specifier to allow for dependency discovery. *)
       let (index, init, _, cond, step, _) = range in
       (* Launch dependency discovery in the initialization term as well as *)
       let (ins, inouts) = trm_discover_dependencies s init in
       (* in the conditional statement representing the upper loop bound. *)
       let (ins', inouts') = trm_discover_dependencies s cond in
       (* Gather the discovered dependencies. *)
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       (* Check whether [step] is formed of a term. In other words, check
          whether it is not simply a unary increment or decrement, but something
          like [i += a * 2]. In this case, *)
       let (ins', inouts') = match step with
         (* we have to look for dependencies in this term. *)
         | Step st -> trm_discover_dependencies s st
         (* Otherwise, we do not have to do nothing, just keep the current in
            and inout dependency sets as they are. *)
         | _ -> (ins, inouts) in
       (* Gather the discovered dependencies, if any. *)
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       (* Create a sub-graph for the body sequenc, i.e. [instr], of the [for]
          loop. *)
       let c = TaskGraph.create() in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let ct = fill s instr c in
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [for] graph node, i.e. [ins] and [inouts],
          by the means of a union operation. *)
       let (ins, inouts) =
         (Dep_set.union ins ct.ins, Dep_set.union inouts ct.inouts) in
       (* If the body sequence contains an unconditional jump, we need to
          propagate this information upwards. *)
       let attrs = if (Task.has_attr ct HasJump) then
                     TaskAttr_set.singleton HasJump
                   else TaskAttr_set.empty in
       (* Create the task corresponding to the current [for] graph node using
          all the elements computed above. *)
       Task.create t attrs scope ins inouts [[c]] 
    | Trm_let _
      | Trm_let_mult _ ->
       (* Keep the state of the local scope from before variable declaration. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the current variable declaration term and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s t in
       (* Convert the updated local scope to a set. *)
       let scope' = var_set_of_var_hashtbl s in
       (* Compute the set of variables that have been declared. *)
       let nv = Var_set.diff scope' scope in
       (* Variable declarations should never become tasks, but rather
          synchronization barriers, nor be merged with other task graph
          nodes. *)
       let attrs = TaskAttr_set.singleton Singleton in
       (* However, the synchronization barrier is needed only when the variable
          declaration depends on previously declared variables, i.e. when the in
          dependency set of the declaration is empty and the in-out dependency
          set contains other variables than those being declared. *)
       let others = Dep_set.filter (
                      fun d -> match (Dep.to_atomic d) with
                               | Dep_var v -> not (Var_set.mem v nv)
                               | Dep_trm (_, v) -> not (Var_set.mem v nv)
                               | _ -> true
                    ) inouts in
       let attrs = if (Dep_set.cardinal ins > 0) &&
                        (Dep_set.cardinal others > 0) then
                     TaskAttr_set.add WaitForSome attrs else
                     TaskAttr_set.add WaitForNone attrs in
       (* Create a barrier corresponding to the current variable declaration
          term. Variable declarations should never appear in tasks. *)
       Task.create t attrs scope' ins inouts []
    | Trm_apps _ ->
       (* Look for dependencies in the current term and initialize the in and
          in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s t in
       (* Convert the local scope to a set. *)
       let scope = var_set_of_var_hashtbl s in
       (* Create the task corresponding to the current graph node using all the
          elements computed above. No task attributes are needed here. *)
       Task.create t TaskAttr_set.empty scope ins inouts []
    | Trm_if (cond, yes, no) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [if] and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* Create sub-graphs for the [then] and the [else] branches. *)
       let gy = TaskGraph.create () in
       let gn = TaskGraph.create () in
       (* Taskify the branches while filling the correspoding sub-graphs. *)
       let ty = fill s yes gy in
       (* If there is no [else] branch, create an empty task. *)
       let missing_tn = is_trm_unit no in 
       let tn = if missing_tn then Task.empty () else fill s no gn in
       (* Include the dependencies from the branches into the sets of
          dependencies of the current [if] graph node, i.e. [ins] and [inouts],
          by the means of union operations. *)
       let (ins, inouts) =
         (Dep_set.union ins ty.ins, Dep_set.union inouts ty.inouts) in
       let (ins, inouts) =
         (Dep_set.union ins tn.ins, Dep_set.union inouts tn.inouts) in
       (* If the [then] or the [else] branch contains an unconditional jump, we
          need to propagate this information upwards. *)
       let attrs = if (Task.has_attr ty HasJump) || (Task.has_attr tn HasJump)
                   then
                     TaskAttr_set.singleton HasJump
                   else
                     TaskAttr_set.empty in
       (* An [if] node should not become a task by itself. *)
       let attrs = TaskAttr_set.add WaitForSome attrs in
       (* Initialize the list of sub-graphs corresponding to the [then] branch
          and, if present, for the [else] branch too. *)
       let children = if missing_tn then [] else [gn] in
       let children = gy :: children in
       (* Create the task corresponding to the current [if] graph node using all
          the elements computed above. *)
       Task.create t attrs scope ins inouts [children]
    | Trm_while (cond, body) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [while] and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* Create a sub-graph for the body sequence of the [while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s body gb in
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [while] graph node, i.e. [ins] and
          [inouts], by the means of a union operation. *)
       let (ins, inouts) =
         (Dep_set.union ins tb.ins, Dep_set.union inouts tb.inouts) in
       (* If the body sequence contains an unconditional jump, we need to
          propagate this information upwards. *)
       let attrs = if (Task.has_attr tb HasJump) then
                     TaskAttr_set.singleton HasJump
                   else TaskAttr_set.empty in
       (* Create the task corresponding to the current [while] graph node using
          all the elements computed above. *)
       Task.create t attrs scope ins inouts [[gb]]
    | Trm_do_while (body, cond) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [do-while]
          and initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* Create a sub-graph for the body sequence of the [do-while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s body gb in
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [do-while] graph node, i.e. [ins] and
          [inouts], by the means of a union operation. *)
       let (ins, inouts) =
         (Dep_set.union ins tb.ins, Dep_set.union inouts tb.inouts) in
       (* If the body sequence contains an unconditional jump, we need to
          propagate this information upwards. *)
       let attrs = if (Task.has_attr tb HasJump) then
                     TaskAttr_set.singleton HasJump
                   else TaskAttr_set.empty in
       (* Create the task corresponding to the current [do-while] graph node
          using all the elements computed above. *)
       Task.create t attrs scope ins inouts [[gb]]
    | Trm_switch (cond, cases) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [switch]
          and initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
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
       (* Include the dependencies from the block sequences into the sets of
          dependencies of the current [switch] graph node, i.e. [ins] and
          [inouts], by the means of union operations. *)
       let (ins, inouts) = List.fold_left (fun (ins', inouts') (tb : Task.t) ->
                               (Dep_set.union ins' tb.ins,
                                Dep_set.union inouts' tb.inouts))
                             (ins, inouts) tbs in
       (* If at least one of the block sequences contains an unconditional jump
          other than [break], we need to propagate this information upwards. *)
       let has_jump = List.exists (fun e -> Task.has_attr e HasJump) tbs in
       let attrs = if has_jump then TaskAttr_set.singleton HasJump
                   else TaskAttr_set.empty in
       (* Create the task corresponding to the current [switch] graph node
          using all the elements computed above. *)
       Task.create t attrs scope ins inouts [gbs]
    | Trm_delete (_, target) ->
       (* Look for dependencies in the target term of the [delete]. [delete] is
          a destructive operation, we need to consider all of the dependencies
          as in-out dependencies, of course. *)
       let (ins, inouts) = trm_discover_dependencies s target in
       let inouts = Dep_set.union ins inouts in
       (* Convert the local scope to a set *)
       let scope = var_set_of_var_hashtbl s in
       (* Transform this task into a synchronization barrier. See
          [Apac_tasks.TaskAttr]. *)
       let attrs = TaskAttr_set.singleton WaitForSome in
       (* in order to be able to use it when creating the task corresponding to
          the current [delete] graph node. *)
       Task.create t attrs scope Dep_set.empty inouts []
    | Trm_goto target ->
       (* If the target label of the [goto] is not the [Apac_core.goto_label] we
          use within the return statement replacement transformation
          [Apac_basic.use_goto_for_return], fail. Other goto statements than
          those we add are not allowed within a taskification target. *)
       if target <> Apac_macros.goto_label then
         fail t.loc "Apac_core.taskify_on.fill: illegal goto statement"
       else
         (* If [target] is [Apac_core.goto_label], we can create a [Task]
            instance for it, even if it will actually nevery become a task. The
            [Singleton] and the [HasJump] attributes mean that the [Task] will
            not be merged with any other task and that it contains an
            unconditional jump. See [Apac_tasks.TaskAttr]. *)
         let attrs = TaskAttr_set.singleton Singleton in
         let attrs = TaskAttr_set.add HasJump attrs in
         Task.create t attrs Var_set.empty Dep_set.empty Dep_set.empty []
    | Trm_val v ->
       (* Retrieve the first label attribute of the current term, if any. *)
       let l = trm_get_labels t in
       let l = if (List.length l) > 0 then List.nth l 0 else "" in
       (* We have to check whether this value term is a goto label arising from
          [Apac_basic.use_goto_for_return]. *)
       begin match v with
         (* If it is the case, we can create a [Task] instance for it, even if
            it will actually nevery become a task. The [Singleton] and the
            [ExitPoint] attributes mean that the [Task] will not be merged with
            any other task and that it represents the exit point of the
            execution sequence. See [Apac_tasks.TaskAttr]. *)
       | Val_lit (Lit_unit) when l = Apac_macros.goto_label ->
          let attrs = TaskAttr_set.singleton Singleton in
          let attrs = TaskAttr_set.add ExitPoint attrs in
          Task.create t attrs Var_set.empty Dep_set.empty Dep_set.empty []
       (* Otherwise, fail. Other types of values are not allowed as first-level
          instructions within a task group. *)
       | _ ->
          let it = AstC_to_c.ast_to_string t in
          let error =
            Printf.sprintf
              "Apac_core.taskify_on.fill: illegal value term '%s'" it in
          fail t.loc error
       end
    | Trm_omp_routine r ->
       (* Convert the local scope to a set. *)
       let scope = var_set_of_var_hashtbl s in
       (* Initialize an empty task attribute set. Calls to OpenMP routines does
          not need any attribute. *)
       let attrs = TaskAttr_set.empty in
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
       let (ins, inouts) = trm_discover_dependencies s (trm_var v) in
       let inouts = Dep_set.union ins inouts in
       (* Create the task corresponding to the current OpenMP routine call graph
          node. *)
       Task.create t attrs scope Dep_set.empty inouts []
       (* 2) On the other hand, all the other routines do not involve any
             variables and thus do not require dependency discovery. *)
       | _ ->
          (* Create the task corresponding to the current OpenMP routine call
             graph node. *)
          Task.create t attrs scope Dep_set.empty Dep_set.empty []
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
  let g' = TaskGraphOper.transitive_reduction g in
  const_record.task_graph <- Some (g');
  TaskGraph.iter_vertex (fun vertex ->
      let lab : Task.t = TaskGraph.V.label vertex in
      Printf.printf "vertex: %s\n" (Task.to_string lab)) g';
  export_task_graph g' "apac_task_graph.dot"
  (*fill const_record.variables t task_graph;
  Printf.printf "Augmented AST for <%s> follows:\n%s\n"
    (var_to_string f) (atrm_to_string aast)*)
    
let taskify (tg : target) : unit =
  Target.iter (fun t p -> taskify_on p (get_trm_at_path p t)) tg

(* [merge_on p t]: see [merge]. *)
let merge_on (p : path) (t : trm) : unit =
  let rec seq (g : TaskGraph.t) (start : TaskGraph.V.t) :
            TaskGraph.V.t list =
    if (TaskGraph.out_degree g start) > 1 then
      begin
        [start]
      end
    else
      begin
        let child = TaskGraph.succ g start in
        if (List.length child) < 1 then
          begin
            [start]
          end
        else
          begin
            let child = List.hd child in
            let chtask = TaskGraph.V.label child in
            if not (Task.has_attr chtask Singleton) &&
                 (TaskGraph.in_degree g child) < 2 then
              start :: (seq g child)
            else [start]
          end
      end
  in
  let rec iter (g : TaskGraph.t) : unit =
    let vs = TaskGraph.fold_vertex (fun v acc -> v::acc) g [] in
    let nb = TaskGraph.nb_vertex g in
    for i = 0 to (nb - 1) do
      begin
        let vi = List.nth vs i in
        let ti = TaskGraph.V.label vi in
        if not (Task.has_attr ti Singleton) &&
             (TaskGraph.mem_vertex g vi) && (TaskGraph.in_degree g vi > 0) then
          begin
            let sq : TaskGraph.V.t list = seq g vi in 
            let st = List.length sq in
            if st > 1 then
              begin
                let start = List.hd sq in
                let tail = List.tl sq in
                let first = TaskGraph.V.label start in
                let task : Task.t = List.fold_left (fun t v ->
                                        let curr : Task.t =
                                          TaskGraph.V.label v in
                                        Task.merge t curr) first tail in
                let stop = List.nth sq (st - 1) in
                let pred = TaskGraph.pred g start in
                let succ = TaskGraph.succ g stop in
                let vi' = TaskGraph.V.create task in
                TaskGraph.add_vertex g vi';
                List.iter (fun v -> TaskGraph.add_edge g v vi') pred;
                List.iter (fun v -> TaskGraph.add_edge g vi' v) succ;
                List.iter (fun v -> TaskGraph.remove_vertex g v) sq
              end
          end
      end
    done;
    TaskGraph.iter_vertex (fun v ->
        let t : Task.t = TaskGraph.V.label v in
        List.iter (fun l -> List.iter (fun g -> iter g) l) t.children) g
  in
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.merge_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_core.merge_on: Missing task graph. Did you \
                          taskify?" in
  iter g;
  export_task_graph g "apac_task_graph_merged.dot"

let merge (tg : target) : unit =
  Nobrace.enter ();
  Target.iter (fun t p -> merge_on p (get_trm_at_path p t)) tg

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
  (* let _ = Debug_transfo.trm "output" result in *)
  result
  
    
let profile_tasks (tg : target) : unit =
  Target.apply (fun t p -> Path.apply_on_path (profile_tasks_on p) t p) tg

let include_apac_profiler_on (p : path) (t : trm) : trm =
  if p <> [] then
    fail t.loc "Apac_core.include_apac_profiler_on: expects to be applied on \
                the root of the AST"
  else
    begin
      match t.desc with
      | Trm_seq tl ->
         let directive = code (Expr "#include \"apac_profiler.hpp\"") in
         let tl' = Mlist.insert_at 0 directive tl in
         trm_seq ~annot:t.annot tl'
      | _ -> fail t.loc "Apac_core.include_apac_profiler_on: expects to be \
                         applied on a sequence"
    end

let include_apac_profiler (tg : target) : unit =
  Target.apply (fun t p ->
      Path.apply_on_path (include_apac_profiler_on p) t p) tg
