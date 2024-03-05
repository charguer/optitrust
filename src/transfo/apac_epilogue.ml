open Ast
open Typ
open Trm
open Mark
open Target
open Apac_miscellaneous
open Apac_dep

(* [trmq]: a persistent FIFO queue of terms. *)
type trmq = trm Queue.t

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
  let one ?(reference = false) ?(mult = false) (deletes : trmq)
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
        let _ = Printf.printf ">>>> Bude deelete\n" in
        let dt = trm_delete (!delete = 2) (trm_var ~kind:vk v) in
        let inout = [Inout [Dep_var v]] in
        let depend = [Depend inout] in
        let clauses = [Default Shared_m] in
        let clauses = clauses @ depend in
        let pragma = Task clauses in
        let dt = trm_add_pragma pragma dt in
        Queue.add dt deletes
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
  let rec delete (deletes : trms) (t : trm) : trm =
    match t.desc with
    | Trm_seq _ ->
       let breakable = trm_has_mark Apac_macros.heapify_breakable_mark t in
       let t' = trm_map (delete deletes) t in
       let error = "[heapify_on.delete]: expected a sequence term." in
       let stmts = trm_inv ~error trm_seq_inv t' in
       let stmts' = match Mlist.findi (fun t -> is_trm_abort t) stmts with
         | Some (idx, abort) when is_return abort || breakable ->
            let abort' = Syntax.trm_seq_no_brace (deletes @ [abort]) in
            Mlist.replace_at idx abort' stmts
         | Some (idx, abort) -> stmts
         | _ ->
            let len = Mlist.length stmts in
            Mlist.insert_sublist_at len deletes stmts
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
  (* Map over the terms in [ml] and perform the heapification of single and
     multiple variable declarations using the
     [Apac_basic.heapify_intro_on.single] function. *)
  let ml = Mlist.map (fun t ->
               match t.desc with
               | Trm_let (kind, (v, ty), init, _) ->
                  let ((v2, ty2), init2) =
                    one ~reference:(trm_has_cstyle Reference t) deletes
                      kind v ty init in
                  trm_let kind (v2, ty2) init2
               | Trm_let_mult (kind, vtys, inits) ->
                  (* To heapify a multiple variable declaration, we loop over
                     all the declarations in the corresponding term. *)
                  let updated = List.map2 (
                                    fun (v, ty) init ->
                                    one ~mult:true deletes kind v ty init
                                  ) vtys inits in
                  (* The multiple variable declaration constructor
                     [trm_let_mult] expects the typed variables and
                     initialization terms in separate lists. *)
                  let (vtys2, inits2) = List.split updated in
                  trm_let_mult kind vtys2 inits2
               | _ -> t) ml in
  (* Transform the [deletes] queue into a list. *)
  let deletes = List.of_seq (Queue.to_seq deletes) in
  let ml = Mlist.map (fun t ->
               List.fold_left (fun acc dt ->
                   match dt.desc with
                   | Trm_delete (arr, tv) when not arr ->
                      begin match tv.desc with
                      | Trm_var (_, v) -> apply_on_pragmas (fun pl ->
                                              (subst_pragmas v tv)
                                                pl) acc
                      | _ -> acc
                      end
                   | _ -> acc) t deletes) ml in
  (* Build an updated the sequence term. *)
  let t' = trm_seq ~annot:t.annot ml in
  let t' = List.fold_left (fun acc dt ->
               match dt.desc with
               | Trm_delete (arr, tv) when not arr ->
                  begin match tv.desc with
                  | Trm_var (_, v) -> trm_subst_var v (trm_get tv) acc
                  | _ -> acc
                  end
               | _ -> acc) t' deletes in
  (* Add the [delete] terms from [deletes] to [t']. *)
  delete deletes t'

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
