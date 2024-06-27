open Ast
open Typ
open Trm
open Mark
open Target
open Apac_miscellaneous
open Apac_records
open Apac_dep
open Apac_tasks
open Apac_backend

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
                         let dl' = List.map (fun dt ->
                                       match dt with
                                       | In dl -> In (aux dl)
                                       | Out dl -> Out (aux dl)
                                       | Inout dl -> Inout (aux dl)
                                       | Outin dl -> Outin (aux dl)
                                       | Sink dl -> Sink (aux dl)
                                       | _ -> dt) dl in
                         Depend dl'
                      | _ -> c) cl'
                else []
      in
      match p with
      | Task _ -> Task cl'
      | Taskwait _ -> Taskwait cl'
      | _ -> p) pl

(* [heapify_on t]: see [Apac_basic.heapify]. *)
let heapify_on (t : trm) : trm =
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
                  let init2 =
                    if is_typ_const tya then
                      begin
                        delete := 2;
                        trm_new ty init
                      end
                    else
                      begin
                        delete := 0;
                        init
                      end
                  in
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
        let fp = new_var (get_apac_variable ApacDepthLocal) in
        let fp = [FirstPrivate [fp]] in
        let co = (get_apac_variable ApacCountOk) ^ " || " ^
                         (get_apac_variable ApacDepthOk) in
        let co = [If co] in
        let inout = Dep_var v in
        let inout = [Inout [inout]] in
        let depend = [Depend inout] in
        let clauses = [Default Shared_m] in
        let clauses = clauses @ depend in
        let clauses = if !Apac_macros.instrument_code then clauses @ fp @ co
                      else clauses in
        let pragma = Task clauses in
        let count_preamble = count_update false in
        let depth_preamble = depth_update () in
        let count_postamble = count_update true in
        let dt = if !Apac_macros.instrument_code then
                   let dt' = [depth_preamble; dt; count_postamble] in
                   trm_seq_nomarks dt'
                 else dt in
        let dt = trm_add_pragma pragma dt in
        let dt = if !Apac_macros.instrument_code then
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

(* [instrument_task_group_on t]: see [instrument_task_group]. *)
let instrument_task_group_on (t : trm) : trm =
  (* Deconstruct the sequence term [t]. *)
  let error = "Apac_epilogue.instrument_task_group_on: expected a target to a \
               sequence" in
  let seq = trm_inv ~error trm_seq_inv t in
  (* Build the definition term of the boolean deciding whether the task spawning
     should be cut off based on the current task count. *)
  let ok1 = code
              (Instr
                 ("int " ^
                    (get_apac_variable ApacCountOk) ^
                      " = " ^
                        (get_apac_variable ApacCountInfinite) ^
                          " || " ^
                            (get_apac_variable ApacCount) ^
                              " < " ^
                                (get_apac_variable ApacCountMax)))
  in
  (* Build the definition term of the local task-private copy of the current
     task depth [ApacDepthLocal] variable. *)
  let local = code
                (Instr
                   ("int " ^
                      (get_apac_variable ApacDepthLocal) ^
                        " = " ^
                          (get_apac_variable ApacDepth)))
  in
  (* Build the definition term of the boolean deciding whether the task spawning
     should be cut off based on the current task depth. *)
  let ok2 = code
              (Instr
                 ("int " ^
                    (get_apac_variable ApacDepthOk) ^
                      " = " ^
                        (get_apac_variable ApacDepthInfinite) ^
                          " || " ^
                            (get_apac_variable ApacDepthLocal) ^
                              " < " ^
                                (get_apac_variable ApacDepthMax)))
  in
  (* Prepend these three terms to the original sequence and *)
  let seq' = [ok1; local; ok2] in
  let seq' = Mlist.of_list seq' in
  let seq' = Mlist.merge seq' seq in
  (* rebuild a new one. *)
  trm_seq ~ctx:t.ctx ~annot:t.annot seq'
  
(* [instrument_task_group tg]: expects the target [tg] to point at a task group
   sequence in which it prepends all the terms with instrumentation terms
   involved in the task granularity control in the resulting source code. *)
let instrument_task_group (tg : target) : unit =
  Target.apply_at_target_paths instrument_task_group_on tg

(* [instrument_unit_on ~backend t]: see [instrument_unit]. *)
let instrument_unit_on ?(backend : task_backend = OpenMP) (t : trm) : trm =
  (* Deconstruct the sequence term [t]. *)
  let error = "Apac_epilogue.instrument_unit_on: expected a target to a \
               sequence" in
  let seq = trm_inv ~error trm_seq_inv t in
  (* Build the definition term of the flag allowing the end-user to disable the
     task creation cut-off based on the number of active tasks. *)
  let ok1 = code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacCountInfinite) ^
                      " = getenv(\"" ^
                        Apac_macros.apac_count_infinite ^
                          "\") ? 1 : 0"))
  in
  (* Build the definition term of the flag allowing the end-user to disable the
     task creation cut-off based on the current task depth. *)
  let ok2 = code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacDepthInfinite) ^
                      " = getenv(\"" ^
                        Apac_macros.apac_depth_infinite ^
                          "\") ? 1 : 0"))
  in
  (* Build the definition term of the parameter allowing the end-user to
     manually set the maximum count of active tasks. *)
  let max1 = code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacCountMax) ^
                      " = getenv(\"" ^
                        Apac_macros.apac_count_max ^
                          "\") ? atoi(getenv(\"" ^
                            Apac_macros.apac_count_max ^
                              "\")) : omp_get_max_threads() * " ^
                                (string_of_int
                                   Apac_macros.apac_count_thread_factor)))
  in
  (* Build the definition term of the parameter allowing the end-user to
     manually set the maximum task depth. *)
  let max2 = code
              (Instr
                 ("const static int " ^
                    (get_apac_variable ApacDepthMax) ^
                      " = getenv(\"" ^
                        Apac_macros.apac_depth_max ^
                          "\") ? atoi(getenv(\"" ^
                            Apac_macros.apac_depth_max ^
                              "\")) : " ^
                                (string_of_int
                                   Apac_macros.apac_depth_max_default)))
  in
  (* Build the definition term of the counter of active tasks. *)
  let c1 = code
             (Instr
                ("int " ^
                   (get_apac_variable ApacCount) ^
                     " = 0"))
  in
  (* Build the definition term of the counter of the current task depth. *)
  let c2 = code
             (Instr
                ("int " ^
                   (get_apac_variable ApacDepth) ^
                     " = 0"))
  in
  (* Prepend these three terms to the original sequence.*)
  let seq' = [ok1; ok2; max1; max2; c1; c2] in
  (* If the backend is OpenMP, we have to include an extra pragma directive to
     make [ApacDepth] thread-private. *)
  let seq' = match backend with
    | OpenMP ->
       let pragma = code
                      (Stmt
                         ("#pragma omp threadprivate(" ^
                            (get_apac_variable ApacDepth) ^
                              ")")) in
       seq' @ [pragma]
    | _ -> seq'
  in
  let seq' = Mlist.of_list seq' in
  let seq' = Mlist.merge seq' seq in
  (* Finally, we rebuild an updated sequence. *)
  trm_seq ~ctx:t.ctx ~annot:t.annot seq'

(* [instrument_unit ~backend tg]: expects the target [tg] to point at the
   top-level sequence in which it prepends all the terms with instrumentation
   terms involved in the task granularity control in the resulting source code.
   The optional argument [backend] controls the target task-based programming
   backend. By default, we consider OpenMP. *)
let instrument_unit ?(backend : task_backend = OpenMP) (tg : target) : unit =
  Target.apply_at_target_paths (instrument_unit_on ~backend) tg

(* [instrument ~backend tgu tgg]: expects the target [tgu] to point at the
   top-level sequence and the target [tgg] to point at a task group sequence.
   The transformation then prepends all the terms in these sequences with
   instrumentation terms involved in the task granularity control in the
   resulting source code. The optional argument [backend] controls the target
   task-based programming backend. By default, we consider OpenMP. *)
let instrument ?(backend : task_backend = OpenMP)
      (tgu : target) (tgg : target) : unit =
  Trace.ensure_header "#include <stdlib.h>";
  Trace.ensure_header "#include <omp.h>";
  instrument_unit ~backend tgu;
  instrument_task_group tgg

(* [reduce_waits_on p t]: see [reduce_waits]. *)
let reduce_waits_on (p : path) (t : trm) : unit =
  let rec waits (g : TaskGraph.t) (v : TaskGraph.V.t) : bool =
    let t = TaskGraph.V.label v in
    let w = (Task.attributed t WaitForSome) ||
              (Task.attributed t WaitForAll) ||
                (Task.attributed t WaitForNone) ||
                  (TaskGraph.in_degree g v < 1) in
    List.fold_left (fun al gl ->
        al && List.fold_left (fun ao go ->
                 ao && TaskGraph.fold_vertex (fun v' av ->
                           av && (waits go v')
                         ) go w
               ) w gl
      ) w t.children
  in
  let rec propagate (v : TaskGraph.V.t) : unit =
    let t = TaskGraph.V.label v in
    t.attrs <- TaskAttr_set.remove WaitForSome t.attrs;
    t.attrs <- TaskAttr_set.remove WaitForAll t.attrs;
    t.attrs <- TaskAttr_set.add WaitForNone t.attrs;
    List.iter (fun g ->
        List.iter (fun g' ->
            TaskGraph.iter_vertex (fun v' ->
                propagate v'
              ) g'
          ) g
      ) t.children
  in
  let rec iter (f : TaskGraph.V.t -> bool)
            (g : TaskGraph.t) (v : TaskGraph.V.t) : unit =
    let me = f v in
    if (TaskGraph.fold_succ (fun v' a -> a && waits g v') g v me) then
      TaskGraph.iter_succ (fun v' -> iter f g v') g v
  in
  let rec prev (f : TaskGraph.V.t -> bool)
            (g : TaskGraph.t) (v : TaskGraph.V.t) : unit =
    let me = f v in
    if (TaskGraph.fold_pred (fun v' a -> a && waits g v') g v me) then
      TaskGraph.iter_pred (fun v' -> prev f g v') g v
  in
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_taskify.join_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_taskify.join_on: Missing task graph. Did you \
                          taskify?" in
  let r = TaskGraphOper.root g in
  let s = TaskGraph.fold_vertex (fun v acc ->
              if TaskGraph.out_degree g v < 1 then v::acc else acc
            ) g [] in
  let s = List.hd s in
  let continue = ref true in
  let last = ref s in
  Printf.printf "Joining '%s'\n" (var_to_string f);
  iter (fun v ->
      if !continue then
        begin
          let w = waits g v in
          if w then propagate v
          else
            begin
              continue := false
            end;
          w
        end
      else false) g r;
  continue := true;
  Printf.printf "Bottom-up '%s'\n" (var_to_string f);
  prev (fun v ->
      if !continue then
        begin
          let w = waits g v in
          if w then
            begin
              propagate v;
              last := v
            end
          else
            begin
    (*          let t = TaskGraph.V.label !last in
              t.attrs <- TaskAttr_set.remove WaitForNone t.attrs;
              t.attrs <- TaskAttr_set.add WaitForAll t.attrs; *)
              continue := false
            end;
          w
        end
      else false) g s;
  TaskGraph.iter_vertex (fun v ->
      if TaskGraph.V.equal v !last then
        let t = TaskGraph.V.label v in
        let _ = Printf.printf "EQUALITY on %s\n" (Task.to_string t) in
        t.attrs <- TaskAttr_set.remove WaitForNone t.attrs;
        t.attrs <- TaskAttr_set.add WaitForAll t.attrs
    ) g;
  let dot = (cwd ()) ^ "/apac_task_graph_" ^ f.name ^ "_join.dot" in
  export_task_graph g dot;
  dot_to_pdf dot

let reduce_waits (tg : target) : unit =
  Target.iter (fun t p -> reduce_waits_on p (Path.get_trm_at_path p t)) tg

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
    if (not (Task.attributed task WaitForAll)) &&
         (not (Task.attributed task WaitForSome)) &&
           (not (Task.attributed task WaitForNone)) then
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
  let scope = Dep_hashtbl.create 97 in
  let subscripts = Stack.create () in
  TaskGraphTraverse.iter (synchronize scope subscripts g) g;
  Stack.iter (fun (d, v, g) -> process d v g) subscripts;
  Printf.printf "Sync. subscripts task graph of << %s >> follows:\n" (var_to_string f);
  TaskGraphPrinter.print g;
  let dot = (cwd ()) ^ "/apac_task_graph_" ^ f.name ^ "_sync.dot" in
  export_task_graph g dot;
  dot_to_pdf dot

let synchronize_subscripts (tg : target) : unit =
  Target.iter (fun t p ->
      synchronize_subscripts_on p (Path.get_trm_at_path p t)) tg

(* [reduce_waits2_on p t]: see [reduce_waits2]. *)
let reduce_waits2_on (p : path) (t : trm) : unit =
  let rec process (previous : (Task.t * int) Stack.t) (level : int)
            (vertex : TaskGraph.V.t) : unit =
    let task : Task.t = TaskGraph.V.label vertex in
    if (Task.taskified task) then
      Stack.push (task, level) previous
    else if (Task.attributed task WaitForSome) then
      begin
        let ins = Dep_set.filter (fun d ->
                      Dep_map.has_with_attribute d Condition task.ioattrs
                    ) task.ins in
        let inouts = Dep_set.filter (fun d ->
                      Dep_map.has_with_attribute d Condition task.ioattrs
                       ) task.inouts in
        let task' : Task.t = {
            current = task.current;
            attrs = task.attrs;
            ins = ins;
            inouts = inouts;
            ioattrs = task.ioattrs;
            children = task.children;
          }
        in
        let depends = Stack.fold (fun acc (t, l) ->
                          acc || (Task.depending t task')
                        ) false previous in
        if (not depends) then
          begin
            task.attrs <- TaskAttr_set.remove WaitForSome task.attrs;
            task.attrs <- TaskAttr_set.add WaitForNone task.attrs
          end
      end;
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter (process previous (level + 1)) go
          ) gl
      ) task.children
  in
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_epilogue.reduce_waits2_on: unable to find \
                          parent function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_epilogue.reduce_waits2_on: Missing task graph. \
                          Did you taskify?" in
  let previous : (Task.t * int) Stack.t = Stack.create () in
  TaskGraphTraverse.iter (process previous 1) g;
  Printf.printf "Reduced waits2 task graph of << %s >> follows:\n" (var_to_string f);
  TaskGraphPrinter.print g;
  let dot = (cwd ()) ^ "/apac_task_graph_" ^ f.name ^ "_waits2.dot" in
  export_task_graph g dot;
  dot_to_pdf dot

let reduce_waits2 (tg : target) : unit =
  Target.iter (fun t p -> reduce_waits2_on p (Path.get_trm_at_path p t)) tg
