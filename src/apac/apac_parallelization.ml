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
    counter at the end of a task, set [postamble] to [true]. Also, by default,
    the target backend is OpenMP. This can be changed through the optional
    [backend] argument. *)
let count_update (postamble : bool) : trm =
  (* Retrieve the string representation of the involved instrumentation
     variables. *)
  let count = get_apac_variable ApacCount in
  let ok = get_apac_variable ApacCountOk in
  (* Decide on the update operation to apply. *)
  let op = if postamble then "--" else "++" in
  (* Build the task count update term. *)
  let update = code (Instr (count ^ op)) in
  (* Prepend it with the OpenMP atomic pragma. *)
  let update = trm_add_pragma (Atomic None) update in
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

(** [get_cutoff]: generates static cut-off condition term. *)
let get_cutoff () : trm =
  let count = trm_var (new_var (get_apac_variable ApacCountOk)) in
  let depth = trm_var (new_var (get_apac_variable ApacDepthOk)) in
  trm_or count depth

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
                trm_array_get (trm_get tv) v0
             | _ -> trm_get tv
           in
           let t' = trm_subst_var va tv' t in
           Dep_trm (t', v)
        | Dep_var v when v = va ->
           let t' = trm_array_get (trm_get tv) v0 in
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

(* [heapify_on t]: see [Apac_basic.heapify]. *)
let heapify_on (t : trm) : trm =
  let open Typ in
  let open Apac_miscellaneous in
  (* [heapify_on.one ?reference ?mult deletes v ty init] promotes a single
     variable declaration, represented by the variable [v] of type [ty] and by
     the initialization term [init], from the stack to the heap. When the [mult]
     parameter is set to [true], one can call the function in a loop on a series
     of declarations composing a multiple variable declaration and process them
     all. The [reference] parameter is useful in the case of a simple variable
     declaration when the reference status has to be determined before calling
     the function as the information cannot be restored from within the function
     like in the case of a multiple variable declaration. *)
  let one ?(reference = false) ?(mult = false)
        (deletes : trmq) (variables : varq)
        (vk : varkind) (v : var) (ty : typ) (init : trm) : typed_var * trm =
    (* Beyond promoting variables to the heap, this function allow for producing
       an adequate [delete] term allowing for future de-allocation of the
       promoted variables. However, the promotion to the heap does not always
       take place, i.e. if the target variable is already on the heap. At the
       end of the process, the [delete] integer will tell us whether the
       production of a [delete] term is required (positive value) or not (zero)
       and whether we are freeing a variable (1) or an array (2). *)
    let delete = ref 1 in
    (* Depending on whether it is a multiple [mult] is [true] or a single
       variable declaration [mult] is [false], the encoding of the type and the
       const status in the OptiTrust AST differs. *)
    let result =
      if mult then
        begin
          (* Acquire the inner type of [ty], e.g. [const int] from [const
             int[5]]. *)
          let tyi = get_inner_type ty in
          (* If we are declaring a reference, *)
          if is_reference ty then
            begin
              (* we have to do something only when it is a constant reference to
                 a literal value, i.e. it is not referencing a previously
                 user-allocated memory location. *)
              if is_typ_const tyi && not (trm_can_resolve_pointer init) then
                (* In the case of a multiple variable declaration, [ty] is not a
                   reference. We have to constify it and assign it the pointer
                   type, not the reference type. Otherwise, we would have to
                   create a reference to a constant pointer. *)
                ((v, typ_ptr Ptr_kind_mut (typ_const ty)), trm_new tyi init)
                  (* Otherwise, we return the declarations elements as they
                     are. *)
              else
                begin
                  delete := 0;
                  ((v, ty), init)
                end
            end
              (* Otherwise, we distinguish four different cases: *)
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
                     points to some data in the heap. If it is not the case,
                     e.g. in [int i = 1; int * a = &i], it is not of the
                     responsibility of this transformation function. Otherwise,
                     we replace the initial [init] an adequate allocation term.
                     
                     Note that, here, we perform the test on the inner type
                     because [ty] is a const type in this case. *)
                  let init2 =
                    if is_typ_ptr tyi then
                      begin
                        delete := 0;
                        init
                      end
                    else trm_new ty init
                  in
                  (* Return the updated variable declaration and definition. *)
                  ((v, ty2), init2)
                end
              (* 2) We are declaring a static array (of values, of pointers,
                 const or not), e.g. [int * a[10]]. *)
              else if is_typ_array ty then
                begin
                  (* To transform a static array allocation to a dynamic array
                     allocation, we need to determine its inner type, i.e. the
                     type of the values stored in the array (without the square
                     brackets), for the construction of the [new <inner-type>]
                     allocation term. Note that if the static variable is an
                     array of constants, e.g. [int const tab[2]], [tya] shall be
                     [const int]. *)
                  let tya = get_inner_array_type tyi in
                  (* We then transform the lvalue type to a pointer, e.g. [int
                     tab[2]] becomes [int * tab]. *)
                  let ty2 = typ_ptr Ptr_kind_mut tya in
                  (* If it is an array of constants, we also need to add [const]
                     to [const int * tab] so as it becomes [const int * const
                     tab]. *)
                  let ty2 = if is_typ_const tya then typ_const ty2 else ty2 in
                  delete := 2;
                  (* Return the updated variable declaration and definition. *)
                  ((v, ty2), trm_new ty init)
                end
              (* 3) We are declaring something that is already a pointer. In
                 this case, we do not transform the variable declaration. *)
              else if is_typ_ptr tyi then
                begin
                  (* However, for some reason, the original allocation term
                     becomes encompassed by a pointer allocation term at some
                     point, so we have to gather the original one back. *)
                  let error = "[heapify_intro_on.single: expected a nested \
                               allocation term." in
                  let (_, init2) = trm_inv ~error trm_new_inv init in
                  delete := 0;
                  ((v, tyi), init2)
                end
              (* 4) Any other cases. Typically, we refer here to static variable
                 declarations and definitions such as [int a = 1] or to variable
                 declarations and definitions that were not fully constified,
                 i.e. where either the value or the memory location is not const
                 or none of the two. *)
              else
                begin
                  (* If the variable is already a pointer, we do not need to
                     transform it to a pointer type. *)
                  let ty2 =
                    if not (is_typ_ptr ty) then
                      typ_ptr Ptr_kind_mut ty else ty in
                  (* If the variable is a pointer, we consider that it already
                     points to some data in the heap. If it is not the case,
                     e.g. in [int i = 1; int * a = &i], it is not of the
                     responsibility of this transformation function.

                     Note that, here, we perform the test on directly [ty] is
                     the outer type is not a const type in this case. *)
                  let init2 = if is_typ_ptr ty then
                                begin
                                  delete := 0;
                                  init
                                end
                              else trm_new ty init in
                  (* Return the updated variable declaration and definition. *)
                  ((v, ty2), init2)
                end
            end
        end
      else
        begin
          (* Acquire the inner type of [ty], e.g. [int] from [int *]. Note that
             in this example, [int *] actually encodes a statically allocated
             [int]. This encoding is present only in the case of simple variable
             declarations. This is why we use [get_inner_ptr_type] here instead
             of [get_inner_type] like in the case of multiple variable
             declarations. *)
          let tyi = get_inner_ptr_type ty in
          (* If we are declaring a reference, *)
          if reference then
            (* we have to apply a get operation on the initialization term.
               Otherwise, OptiTrust shall add a [&] operator to the latter, e.g.
               [const int &b = 1] would become [const int &b = &1]. This
               behavior is present only in the case of a single variable
               declaration and is independent of the condition below. *)
            let init2 = trm_get init in
            begin
              (* Here, we have to do something only when it is a constant
                 reference to a literal value, i.e. it is not referencing a
                 previously user-allocated memory location. *)
              if is_typ_const tyi && not (trm_can_resolve_pointer init) then
                (* In the case of a simple variable declaration, [ty] is already
                   a reference. We only have to constify it. *)
                ((v, typ_const ty), trm_new tyi init2)
              else
                begin
                  delete := 0;
                  (* The above affirmation is true only in the case of constant
                     references. For mutable references, we have to restore the
                     reference type of [tyi]. Otherwise, a [int &e = i] results
                     in [int * e = i] even if the heapification is not
                     performed. *)
                  ((v, typ_ptr Ptr_kind_ref tyi), init2)
                end
            end
              (* Otherwise, we distinguish four different cases: *)
          else
            begin
              (* 1) The value as well as the memory location are const, e.g. in
                 [int const * a = &i, * const b = &j] it is the case of [b]. *)
              if is_typ_const ty then
                begin
                  (* In the case of a simple variable declaration, we acquire
                     the inner const type directly from [ty] unlike in the case
                     of a multiple variable declaration (see above). *)
                  let tyc = get_inner_const_type ty in
                  (* If the variable is already a pointer, we do not need to
                     transform it to a pointer type. *)
                  let ty2 =
                    if is_typ_ptr tyc then ty else typ_ptr Ptr_kind_mut ty in
                  (* Then, we have to restore the const status of the
                     variable, if needed. *)
                  let ty2 = if is_typ_const ty2 then ty2 else typ_const ty2 in
                  (* If the variable is a pointer, we consider that it already
                     points to some data in the heap. If it is not the case,
                     e.g. in [int i = 1; int * a = &i], it is not of the
                     responsibility of this transformation function. Otherwise,
                     we replace the initial [init] an adequate allocation term.

                     Note that, here, we perform the test on the inner type
                     because [ty] is a const type in this case. *)
                  let init2 =
                    if is_typ_ptr tyc then
                      begin
                        delete := 0;
                        init
                      end
                    else trm_new ty init in
                  (* Return the updated variable declaration and definition. *)
                  ((v, ty2), init2)
                end
              (* 2) We are declaring a static array (of values, of pointers,
                 const or not), e.g. [int * a[10]]. *)
              else if is_typ_array tyi then
                begin
                  (* To transform a static array allocation to a dynamic array
                     allocation, we need to determine its inner type, i.e. the
                     type of the values stored in the array (without the square
                     brackets), for the construction of the [new <inner-type>]
                     allocation term. Note that if the static variable is an
                     array of constants, e.g. [int const tab[2]], [tya] shall be
                     [const int]. *)
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
                  delete := 2;
                  let init2 =
                    if is_typ_const tya then trm_new ty init else init in
                  (* Return the updated variable declaration and definition. *)
                  ((v, ty2), init2)
                end
              (* 3) We are declaring something that is already a pointer. In
                 this case, we do not transform the variable declaration. *)
              else if is_typ_ptr tyi then
                begin
                  (* However, for some reason, the original allocation term
                     becomes encompassed by a pointer allocation term at some
                     point, so we have to gather the original one back. *)
                  let error = "[heapify_intro_on.single: expected a nested \
                               allocation term." in
                  let (_, init2) = trm_inv ~error trm_new_inv init in
                  delete := 0;
                  ((v, tyi), init2)
                end
              (* 4) Any other cases. Typically, we refer here to static variable
                 declarations and definitions such as [int a = 1] or to variable
                 declarations and definitions that were not fully constified,
                 i.e. where either the value or the memory location is not const
                 or none of the two. *)
              else
                begin
                  (* We then transform the lvalue type to a pointer, e.g. [int
                     a] becomes [int * a]. *)
                  let ty2 = typ_ptr Ptr_kind_mut tyi in
                  (* Then, we have to restore the const status of the variable
                     if it was present in the original inner type [tyi]. *)
                  let ty2 = if is_typ_const tyi then typ_const ty2 else ty2 in
                  (* The transformation of [init] to an adequate allocation term
                     is required only in the case of an array of constants.
                     Otherwise, it is done implicitly by OptiTrust. *)
                  let init2 =
                    if is_typ_const tyi then
                      trm_new ty init
                    else
                      begin
                        delete := 1;
                        init
                      end
                  in
                  (* Return the updated variable declaration and definition. *)
                  ((v, ty2), init2)
                end
            end
        end
    in
    (* If necessary, produce [delete] term allowing for deallocating the
       promoted variable and add them to the [deletes] queue. *)
    if !delete > 0 then
      begin
        let vt = trm_var ~kind:vk v in
        let dt = trm_delete (!delete = 2) vt in
        let fp = new_var (Apac_macros.get_apac_variable ApacDepthLocal) in
        let fp = [FirstPrivate [fp]] in
        let co = [If (get_cutoff ())] in
        let inout = Dep_var v in
        let inout = [Inout [inout]] in
        let depend = [Depend inout] in
        let clauses = [Default Shared_m] in
        let clauses = clauses @ depend in
        let clauses = if !Apac_flags.cutoff_count_and_depth then
                        clauses @ fp @ co
                      else clauses in
        let pragma = Task clauses in
        let count_preamble = count_update false in
        let depth_preamble = depth_update () in
        let count_postamble = count_update true in
        let dt = if !Apac_flags.cutoff_count_and_depth then
                   let dt' = [depth_preamble; dt; count_postamble] in
                   trm_seq_nomarks dt'
                 else dt in
        let dt = trm_add_pragma pragma dt in
        let dt = if !Apac_flags.cutoff_count_and_depth then
                   Syntax.trm_seq_no_brace [count_preamble; dt]
                 else dt in
        Queue.add dt deletes;
        if !delete <> 2 then Queue.add (v, vt) variables;
      end;
    result
  in
  (* [Apac_basic.heapify_on.delete]: an auxiliary function to place the [delete]
     terms from [deletes]
     - before the [return] term of the current sequence term [t] or the end of
       the current sequence if no [return] term is present,
     - before each [return] term in the scope of the current sequence term [t],
     - before [break] and [continue] statements in the current sequence term [t]
       if it is the body of a loop or a case of a [switch] statement (see the
       [breakable] flag). *)
  let rec delete (deletes : trms) (level : int) (t : trm) : trm =
    match t.desc with
    | Trm_seq _ ->
       let breakable = trm_has_mark Apac_macros.heapify_breakable_mark t in
       let t' = trm_map (delete deletes (level + 1)) t in
       let error = "[heapify_on.delete]: expected a sequence term." in
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
  (* Deconstruct the sequence term [t] into the [Mlist] of terms [ml]. *)
  let error = "Apac_basic.heapify_on: expected a target to a sequence." in
  let ml = trm_inv ~error trm_seq_inv t in
  (* Initialize a queue for the [delete] terms allowing for deallocating the
     promoted variables.*)
  let deletes = Queue.create () in
  (* Initialize a queue for the deleted variables.*)
  let variables = Queue.create () in
  (* Map over the terms in [ml] and perform the heapification of single and
     multiple variable declarations using the
     [Apac_basic.heapify_intro_on.single] function. *)
  let ml = Mlist.map (fun t ->
               match t.desc with
               | Trm_let (kind, (v, ty), init, _) ->
                  let ((v2, ty2), init2) =
                    one ~reference:(trm_has_cstyle Reference t) deletes
                      variables kind v ty init in
                  trm_let kind (v2, ty2) init2
               | Trm_let_mult (kind, vtys, inits) ->
                  (* To heapify a multiple variable declaration, we loop over
                     all the declarations in the corresponding term. *)
                  let updated = List.map2 (
                                    fun (v, ty) init ->
                                    one ~mult:true deletes variables
                                      kind v ty init
                                  ) vtys inits in
                  (* The multiple variable declaration constructor
                     [trm_let_mult] expects the typed variables and
                     initialization terms in separate lists. *)
                  let (vtys2, inits2) = List.split updated in
                  trm_let_mult kind vtys2 inits2
               | _ -> t) ml in
  (* Transform the [deletes] queue into a list. *)
  let deletes = List.of_seq (Queue.to_seq deletes) in
  (* Transform the [variables] queue into a list. *)
  let variables = List.of_seq (Queue.to_seq variables) in
  let ml = Mlist.map (fun t ->
               List.fold_left (fun acc (v, tv) ->
                  apply_on_pragmas (fun pl -> (subst_pragmas v tv) pl) acc
                 ) t variables) ml in
  (* Build an updated the sequence term. *)
  let t' = trm_seq ~annot:t.annot ml in
  let t' = List.fold_left (fun acc (v, tv) ->
               trm_subst_var v (trm_get tv) acc
             ) t' variables in
  (* Add the [delete] terms from [deletes] to [t']. *)
  delete deletes 0 t'

(* [heapify tg]: expects the target [tg] to point at a sequence in which it
   promotes variables delacred on the stack to the heap. It applies on each
   simple and on each multiple variable declaration that is not a reference or a
   pointer to a previously user-allocated memory location.

   Example:

   int tab[5] = { 1, 2, 3, 4, 5 };

   becomes:

   int * tab = new int[5] { 1, 2, 3, 4, 5 }

   However, in:

   int * a = new int(10);
   int &b = &a;

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
    let _ = Printf.printf "processing successors of: %s\n" (Task.to_string v') in
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
  (** [place_barriers_on.find g]: looks in the task candidate graph [g] for a
      task candidate immediately preceding the task candidate which is either
      itself the first eligible task candidate or the first task candidate to
      involve an eligible task candidate (in a nested task candidate graph). *)
  let find (g : TaskGraph.t) : TaskGraph.V.t option =
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
        visited vertex ([None] in the initial call to the function). *)
    let rec core (p : TaskGraph.V.t option)
              (l : TaskGraph.V.t list) : TaskGraph.V.t option =
      match l with
      | t :: tl -> if (taskifiable t) then p else core (Some t) tl
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
    core None vs 
  in
  (** [place_barriers_on.process l c ts v]: auxiliary function for placing a
      bariier in front of a vertex [v] depending on the presence of preceding
      eligible task candidates in the stack [ts]. [l] represents the task
      candidate immediately following the last eligible task candidate in the
      task candidate graph we apply the function on. When [v] equals [p], place
      a global synchronization barrier on the latter and set [c] to [true] so as
      to prevent the function from placing further barriers. *)
  let rec process (l : TaskGraph.V.t) (c : bool ref) (ts : Task.t Stack.t)
            (v : TaskGraph.V.t) : unit =
    (** Retrieve the label [t] of the task candidate [v]. *)
    let t : Task.t = TaskGraph.V.label v in
    (** If [t] indicates that [v] is an eligible task candidate, i.e. it carries
        the [Taskifiable] attribute, add it to the stack of preceding eligible
        task candidates [ts]. *)
    if (Task.attributed t Taskifiable) then
      Stack.push t ts
    else if (TaskGraph.V.equal l v) then
      begin
        (** If [v] is the task candidate [l] immediately following the last
            eligible task candidate in the target task candidate graph, place a
            global synchronization barrier on the latter and *)
        t.attrs <- TaskAttr_set.add WaitForAll t.attrs;
        (** set [c] to [true] so as to prevent the function from placing further
            barriers. *)
        c := true
      end
    else if (Stack.length ts) > 0 && !c = false then
      (** Otherwise, if, according to [t], [v] does not represent a global
          synchronization barrier (see the [WaitForAll] attribute) nor
          [Apac_macros.goto_label] (see the [ExitPoint] attribute) nor a jump to
          the latter (see the [IsJump] attribute), we have to determine whether
          [v] depends on a preceding eligible task candidate, if any. *)
      if not (Task.attributed t WaitForAll) &&
           not (Task.attributed t ExitPoint)  &&
             not (Task.attributed t IsJump) then
        begin
          (** To this end, we imagine a temporary task candidate with a label
              [temp], based on [t], but omitting the dependencies from nested
              task candidate graphs, if any. We process the task candidates in
              nested task candidate graphs separately through a recurive call to
              this function (see below). *)
          let temp = Task.copy t in
          temp.ins <- if t.children <> [[]] then
                        Dep_set.filter (fun d ->
                            Dep_map.has_with_attribute d Condition t.ioattrs
                          ) t.ins
                      else t.ins;
          temp.inouts <- if t.children <> [[]] then
                           Dep_set.filter (fun d ->
                               Dep_map.has_with_attribute d Condition t.ioattrs
                             ) t.inouts
                         else t.inouts;
          (** Check whether the temporary task candidate shares a data
              dependency with a preceding eligible task candidate. *)
          let depends = Stack.fold (fun acc task' ->
                            acc || (Task.depending task' temp)
                          ) false ts in
          (** If so, we request a synchronization barrier for [v] through [t]
              thanks to the [WaitForSome] attribute. *)
          if depends then t.attrs <- TaskAttr_set.add WaitForSome t.attrs
        end
      else if (Task.attributed t IsJump) then
        (** However, a jump to [Apac_macros.goto_label] (see the [IsJump]
            attribute) requires a global synchronization barrier if any eligible
            task candidate precedes it in the task candidate graph. *)
        begin
          t.attrs <- TaskAttr_set.add WaitForAll t.attrs
        end;
    (** Process the task candidates in nested task candidate graphs, if any. *)
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter_schedule (process l c ts) go
          ) gl
      ) t.children
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
  let bl : TaskGraph.V.t option = find r.graph in
  match bl with
  | Some e ->
     (** Initialize a stack of preceding eligible task candiates. *)
     let ts : Task.t Stack.t = Stack.create () in
     (** Process each task candidate in [r.graph]. *)
     let stop = ref false in
     TaskGraphTraverse.iter_schedule (process e stop ts) r.graph;
     (** Dump the resulting task candidate graph, if requested. *)
     if !Apac_flags.verbose then
       begin
         Printf.printf "Task candidate graph of `%s' (with barriers):\n"
           (var_to_string f);
         TaskGraphPrinter.print r.graph
       end;
     if !Apac_flags.keep_graphs then
       TaskGraphExport.to_pdf r.graph (Apac_macros.gf ~suffix:"barriers" f)
  | None ->
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
          let firstprivate = if !Apac_flags.cutoff_count_and_depth then
                               depth_local_var :: firstprivate
                             else firstprivate in
          let firstprivate = if (List.length firstprivate) > 0 then
                               [FirstPrivate firstprivate]
                             else [] in
          let clauses = shared @ depend in
          let clauses = clauses @ firstprivate in
          let clauses =
            match (
              !Apac_flags.profile && (Option.is_some t.cost),
              !Apac_flags.cutoff_count_and_depth
            ) with
            | (true, true) ->
               let count_and_depth = get_cutoff () in
               let execution_time =
                 trm_gt
                   (Option.get t.cost)
                   (trm_var (new_var (get_apac_variable ApacCutOff))) in
               let cutoff = trm_and count_and_depth execution_time in
               clauses @ [If cutoff]
            | (true, false) ->
               let cutoff =
                 trm_gt
                   (Option.get t.cost)
                   (trm_var (new_var (get_apac_variable ApacCutOff))) in
               clauses @ [If cutoff]
            | (false, true) ->
               clauses @ [If (get_cutoff ())]
            | (false, false) -> clauses
          in
          let pragma = Task clauses in
          let count_preamble = count_update false in
          let depth_preamble = depth_update () in
          let count_postamble = count_update true in
          let instr = if !Apac_flags.cutoff_count_and_depth then
                        depth_preamble :: t.current
                      else t.current in
          let instr = if !Apac_flags.cutoff_count_and_depth then
                        instr @ [count_postamble]
                      else instr in
          let instr = if not (!Apac_flags.cutoff_count_and_depth) &&
                           (List.length instr < 2) then
                        List.hd instr
                      else trm_seq_nomarks instr in
          let instr = trm_add_pragma pragma instr in
          let instr = if sync <> [] then
                        trm_add_pragma (Taskwait sync) instr
                      else instr in
          if !Apac_flags.cutoff_count_and_depth then [count_preamble; instr]
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

(** [cutoff_count_and_depth tg]: expects the target [tg] to point at a task
    group, i.e. a statement sequence with the
    [!Apac_macros.candidate_body_mark]. It then extends the sequence with the
    definitions of function-local variables for controlling task granularity
    according to the number of submitted tasks and the parallelism depth. The
    pass also adds to the abstract syntax tree of the input program the
    [#include] directives and definitions of global variables involved in this
    granularity control mechanism.

    For example, let us consider the following C source code.

    {[
    void f(int * tab) { tab[0] += 42; }

    void p(int & v) { int a = 15; int b = a + 2; int c = a + b + v++; }

    void c(int * tab, int size) {
    /*@__apac_candidate_body*/ {
        f(tab);
        for(int i = 0; i < size; i++) {
          p(tab[i]);
        }
      } /*@__apac_candidate_body*/
    }
    ]}

    Here, the sequence wrapping the body of the function [c] carries the
    [!Apac_macros.candidate_body_mark]. In the first place, the pass adds to the
    beginning of this sequence the definitions of [ApacCountOk],
    [ApacDepthLocal] and [ApacDepth] (see enumeration
    [!type:Apac_macros.apac_variable]).

    {[
    void f(int * tab) { tab[0] += 42; }

    void p(int & v) { int a = 15; int b = a + 2; int c = a + b + v++; }

    void c(int * tab, int size) {
    /*@__apac_candidate_body*/ {
        int __apac_count_ok =
          __apac_count_infinite || __apac_count < __apac_count_max;
        int __apac_depth_local = __apac_depth;
        int __apac_depth_ok =
          __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
        f(tab);
        for(int i = 0; i < size; i++) {
          p(tab[i]);
        }
      } /*@__apac_candidate_body*/
    }
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

    void c(int * tab, int size) {
    /*@__apac_candidate_body*/ {
        int __apac_count_ok =
          __apac_count_infinite || __apac_count < __apac_count_max;
        int __apac_depth_local = __apac_depth;
        int __apac_depth_ok =
          __apac_depth_infinite || __apac_depth_local < __apac_depth_max;
        f(tab);
        for(int i = 0; i < size; i++) {
          p(tab[i]);
        }
      } /*@__apac_candidate_body*/
    }
    ]}

    Note that we insert the statements updating and referring to the variables
    we introduce within the [!parallelize] pass responsible for the generation
    of parallel source code with OpenMP task-based programming pragmas. The
    [cutoff_count_and_depth] pass is responsible only for declaring,
    initializing and configuring the granularity control variables. *)
let cutoff_count_and_depth (tg : target) : unit =
  (** Define a common error message. *)
  let error = "Apac_parallelization.cutoff_count_and_depth: expected a target \
               to a sequence" in
  (** Introduce the function-local granularity control variables. *)
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the target sequence term [t] into an [!module:Mlist] of
          statement terms [s]. *)
      let s = trm_inv ~error trm_seq_inv t in
  (** Build the definition of [ApacCountOk] relying on the globals
      [ApacCountInfinite], [ApacCount] and [ApacCountMax] (see enumeration
      [!type:Apac_macros.apac_variable]). *)
      let open Apac_macros in 
      let ok1 = code
                  (Instr
                     ("int " ^ (get_apac_variable ApacCountOk) ^
                        " = " ^ (get_apac_variable ApacCountInfinite) ^
                          " || " ^ (get_apac_variable ApacCount) ^
                            " < " ^ (get_apac_variable ApacCountMax))) in
      (** Build the definition of [ApacDepthLocal] relying on the global
          [ApacDepth] (see enumeration [!type:Apac_macros.apac_variable]). *)
      let local = code
                    (Instr
                       ("int " ^ (get_apac_variable ApacDepthLocal) ^
                          " = " ^ (get_apac_variable ApacDepth))) in
      (** Build the definition of [ApacDepthOk] relying on the globals
          [ApacDepthInfinite], [ApacDepthLocal] and [ApacDepthMax] (see
          enumeration [!type:Apac_macros.apac_variable]). *)
      let ok2 = code
                  (Instr
                     ("int " ^ (get_apac_variable ApacDepthOk) ^
                        " = " ^ (get_apac_variable ApacDepthInfinite) ^
                          " || " ^ (get_apac_variable ApacDepthLocal) ^
                            " < " ^ (get_apac_variable ApacDepthMax))) in
      (** Insert the definition statements at the beginning of [s] and *)
      let s = Mlist.insert_sublist_at 0 [ok1; local; ok2] s in
      (** re-build [s]. *)
      trm_seq ~ctx:t.ctx ~annot:t.annot s
    ) tg;  
  (** Introduce the global granularity control variables. *)
  Target.apply_at_target_paths (fun t ->
      (** Deconstruct the sequence term [t] representing the root of the
          abstract syntax tree of the input program into an [!module:Mlist] of
          statement terms [s]. *)
      let s = trm_inv ~error trm_seq_inv t in
      (** Build the definition of [ApacCountInfinite] (see enumeration
          [!type:Apac_macros.apac_variable]) relying on the environment variable
          [!Apac_macros.count_infinite]. *)
      let open Apac_macros in
      let ok1 = code
                  (Instr
                     ("const static int " ^
                        (get_apac_variable ApacCountInfinite) ^
                          " = getenv(\"" ^ Apac_macros.count_infinite ^
                            "\") ? 1 : 0")) in
      (** Build the definition of [ApacDepthInfinite] (see enumeration
          [!type:Apac_macros.apac_variable]) relying on the environment variable
          [!Apac_macros.depth_infinite]. *)
      let ok2 = code
                  (Instr
                     ("const static int " ^
                        (get_apac_variable ApacDepthInfinite) ^
                          " = getenv(\"" ^ Apac_macros.depth_infinite ^
                            "\") ? 1 : 0")) in
      (** Build the definition of [ApacCountMax] (see enumeration
          [!type:Apac_macros.apac_variable]) relying on the environment variable
          [!Apac_macros.count_max]. *)
      let max1 = code
                   (Instr
                      ("const static int " ^
                         (get_apac_variable ApacCountMax) ^
                           " = getenv(\"" ^ Apac_macros.count_max ^
                             "\") ? atoi(getenv(\"" ^ Apac_macros.count_max ^
                               "\")) : omp_get_max_threads() * " ^
                                 (string_of_int
                                    !Apac_flags.count_max_thread_factor))) in
      (** Build the definition of [ApacDepthMax] (see enumeration
          [!type:Apac_macros.apac_variable]) relying on the environment variable
          [!Apac_macros.depth_max]. *)
      let max2 = code
                   (Instr
                      ("const static int " ^ (get_apac_variable ApacDepthMax) ^
                         " = getenv(\"" ^ Apac_macros.depth_max ^
                           "\") ? atoi(getenv(\"" ^ Apac_macros.depth_max ^
                             "\")) : " ^
                               (string_of_int
                                  !Apac_flags.depth_max_default))) in
      (** Build the definition of [ApacCount] (see enumeration
          [!type:Apac_macros.apac_variable]). *)
      let c1 = code (Instr ("int " ^ (get_apac_variable ApacCount) ^ " = 0")) in
      (** Build the definition of [ApacDepth] (see enumeration
          [!type:Apac_macros.apac_variable]). *)
      let c2 = code (Instr ("int " ^ (get_apac_variable ApacDepth) ^ " = 0")) in
      (** Make [ApacDepth] (see enumeration [!type:Apac_macros.apac_variable])
          thread-private. *)
      let pragma = code (Stmt ("#pragma omp threadprivate(" ^
                                 (get_apac_variable ApacDepth) ^ ")")) in
      (** Insert the definition statements at the beginning of [s] and *)
      let s =
        Mlist.insert_sublist_at 0 [ok1; ok2; max1; max2; c1; c2; pragma] s in
      (** re-build [s]. *)
      trm_seq ~ctx:t.ctx ~annot:t.annot s
    ) [];
  (** Ensure the [#include] directives for [stdlib.h] and [omp.h]. *)
  Trace.ensure_header "#include <stdlib.h>";
  Trace.ensure_header "#include <omp.h>"

(** [cutoff_execution_time ()]: adds to the abstract syntax tree of the input
    program the definition of a global variable allowing for task granularity
    control relying on execution time models as well as the definition of a
    power computing function the latter may resort to in its formulas.

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
    function. *)
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
          of the environment variable [!Apac_macros.execution_time_cutoff], if
          set, or to the default value [!Apac_macros.execution_time_min]. *)
      let init = code (Expr
                         ("getenv(\"" ^ Apac_macros.execution_time_cutoff ^
                            "\") ? atof(getenv(\"" ^
                              Apac_macros.execution_time_cutoff ^
                                "\")) : " ^ Apac_macros.execution_time_min)) in
      let co = new_var (Apac_macros.get_apac_variable ApacCutOff) in
      let co = trm_let_immut (co, Typ.typ_double ()) init in
      (** Include the definition [!Apac_macros.pow] of the function
          [!Apac_macros.model_pow] to compute powers. *)
      let pow = code (Stmt Apac_macros.pow) in
      (** Insert the definitions [co] and [pow] at the beginning of [s] and *)
      let s = Mlist.insert_sublist_at 0 [co; pow] s in
      (** re-build [s]. *)
      trm_seq ~ctx:t.ctx ~annot:t.annot s
    ) []