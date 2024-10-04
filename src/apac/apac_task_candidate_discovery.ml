open Ast
open Trm
open Mark
open Path
open Target
open Tools
open Apac_records
open Apac_miscellaneous
open Apac_dep
open Apac_tasks

(** [discover_dependencies scope aliases t]: searches the term [t] for data
    dependencies and returns a set of input dependencies, a set of input-output
    dependencies and a map of dependencies to set of dependency attributes.
    During the discovery process, the function takes into account the local
    [scope] of [t] as well as the hash table of [aliases]. *)
let discover_dependencies
      (scope : FunctionRecord.s) (aliases : var Var_Hashtbl.t)
      (t : trm) : (Dep_set.t * Dep_set.t * ioattrs_map) =
  (** [discover_dependencies.may_update_aliases r tv ti]: checks in the hash
      table of aliases [aliases] whether the definition of the variable [v] of
      type [ty] using the initializaion term [init] creates an alias to an
      existing variable. If so, the function updates [aliases]. The [r] flag
      indicates whether [tv] is a reference or not. *)
  let may_update_aliases (r : bool) (v : var) (ty : typ) (init : trm) : unit =
    let open Typ in
    (** We may be creating an alias only if [v] is a reference or a pointer. *)
    if r || is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
      (** In this case, we need to go through any [new] operations,
          referencements or dereferencements in [init] to try to determine the
          target variable [tg] we may be aliasing (see
          [!trm_strip_and_get_lvar]). *)
      let tg =
        match (trm_new_inv init) with
        | Some (_, v') -> v'
        | None -> init in
      let tg = trm_strip_and_get_lvar tg in
      if Option.is_some tg then
        let tg = Option.get tg in
        let tg = tg.v in
        (** If [tg] is in [aliases], it is itself an alias to an existing
            variable [tg'], *)
        if Var_Hashtbl.mem aliases tg then
          (** we are about to create an alias to [tg']. *)
          let tg' = Var_Hashtbl.find aliases tg in
          Var_Hashtbl.add aliases v tg'
        else
          (** Otherwise, [v] becomes the first alias to [tg]. *)
          Var_Hashtbl.add aliases v tg
  in
  (** [discover_dependencies.best_effort ins inouts dam access iao t]: a
      best-effort dependency discovery for language constructs we do not fully
      support yet such as structure member accesses. For the main dependency
      discovery see [!discover_dependencies.main]. The hereby function
      recursively builds and returns [ins], a set of input data dependencies in
      [t], [inouts], a set of input-output data dependencies in [t], [dam], a
      map of dependencies to set of dependency attributes. [access] classifies
      the data dependency either as [In], an input dependency, or [InOut], an
      input-output dependency. Finally, the [iao] flag indicates whether the
      data dependency arises from within an [i]ndex [a]rray [o]perator. *)
  let rec best_effort (ins : Dep_set.t) (inouts : Dep_set.t) (dam : ioattrs_map)
            (access : [ `In | `InOut ]) (iao : bool)
            (t : trm) : (Dep_set.t * Dep_set.t * ioattrs_map) =
    match t.desc with
    (** When [t] is unary operation, e.g. a structure member access or a
        dereferencement, try to determine the underlying variable. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       (** Note that if the unary operation is a unary increment or decrement,
           we must classify the access as input-output. *)
       let access =
         if (is_prefix_unary op) || (is_postfix_unary op) then
           `InOut else access in
       best_effort ins inouts dam access iao t
    (** When [t] is an array access, *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, _) ->
       let (base, accesses) = get_nested_accesses t in
       (** try to determine the [base] variable as well as *)
       let ins, inouts, dam =
         best_effort ins inouts dam access iao base in
       (** the dependencies potentially arising from the [accesses] within
           index array operators. *)
       List.fold_left (fun (ins, inouts, dam) a ->
           match a with
           | Array_access_get t
             | Array_access_addr t -> best_effort ins inouts dam `In true t
           | _ -> (ins, inouts, dam)
         ) (ins, inouts, dam) accesses
    (** When [t] is another binary operation, recurse on both left and
        right-hand sides, i.e. [lhs] and [rhs], so as to see whether there are
        some data dependencies. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ },
                [lhs; rhs]) ->
       let ins, inouts, dam = best_effort ins inouts dam access iao lhs in
       best_effort ins inouts dam access iao rhs
    (** When [t] leads to a variable [v], add the dependency arising from [v]
        into either the set of input or the set of input-output dependencies
        according to its [access] classification. *)
    | Trm_var (_, v) ->
       (** If [v] is an alias to an existing variable, make [v] become the
           latter. *)
       let v, _ = Var_Hashtbl.find_or_default aliases v v in
       (** Look for [v] in the local [scope]. If [v] is not in the local
           scope, set [e] to [false] and suppose that [nli] is null, i.e. that
           [v] is a simple variable, not a pointer or a reference. *)
       let (_, e) = Var_Hashtbl.find_or_default scope v 0 in
       (** If [v] is not in the local scope or if it is a global variable we
           know the definition of, i.e. if [v] is present in
           [!Apac_records.globals], add [GlobalVariable] to the dependency
           attribute set [das] we are about to map to the dependency on [v]. *)
       let das =
         if (not e) || (Var_map.mem v !Apac_records.globals) then
           DepAttr_set.singleton GlobalVariable
         else
           DepAttr_set.empty in
       (** If the dependency appears within an index array operator, we
           must attribute it with [Accessor]. *)
       let das = if iao then DepAttr_set.add Accessor das else das in
       (** Transform [v] into a data dependency [d], *)
       let d = Dep_var v in
       (** add it to the adequate dependency set according to the [access]
           qualifier and update [dam], the map of dependencies to sets of
           dependency attributes, if necessary, i.e. if [das] contains at least
           one new dependency attribute to go with [d]. *)
       let dam =
         if (DepAttr_set.is_empty das) then dam else Dep_map.add d das dam in
       begin match access with
       | `In -> (Dep_set.add d ins, inouts, dam)
       | `InOut -> (ins, Dep_set.add d inouts, dam) end
    (** This is the best we can do. When [t] does not fit any of the above
        cases, finish the discovery process. *)
    | _ -> (ins, inouts, dam)
  in
  (** [discover_dependencies.main ins inouts dam gets call access iao t]: the
      main dependency discovery. It recursively builds and returns [ins], a set
      of input data dependencies in [t], [inouts], a set of input-output data
      dependencies in [t], [dam], a map of dependencies to set of dependency
      attributes.

      When [t] represents a dereferencement, [gets] gives the number of
      dereferencement, or get, operations. The [call] flag indicates whether [t]
      is a part of a function call. [access] classifies the data dependency
      either as [In], an input dependency, or [InOut], an input-output
      dependency. Finally, the [iao] flag indicates whether the data dependency
      arises from within an [i]ndex [a]rray [o]perator. *)
  let rec main (ins : Dep_set.t) (inouts : Dep_set.t) (dam : ioattrs_map)
            (gets : int) (call : bool) (access : [ `In | `InOut ]) (iao : bool)
            (t : trm) : (Dep_set.t * Dep_set.t * ioattrs_map) =
    let error =
      Printf.sprintf
        "Apac_task_candidate_discovery.discover_dependencies.main: '%s' or \
         '%s' is not a valid OpenMP depends expression"
        (AstC_to_c.ast_to_string t)
        (Ast_to_text.ast_to_string t) in
    let warning (t : trm) : unit =
      Printf.printf "WARNING: the dependency discovery does not recognize the \
                     expression `%s', proceeding with a best-effort analysis.\n"
        (AstC_to_c.ast_to_string t)
    in
    (** The behavior of the dependency discovery varies according to the nature
        of [t]. *)
    match t.desc with
    (** [t] is a direct variable access to a variable [v] of kind [vk]. *)
    | Trm_var (vk, v) ->
       (** If [v] is an [alias] to an existing variable, make [v] become the
           latter. *)
       let v, alias = Var_Hashtbl.find_or_default aliases v v in
       (** OptiTrust considers calls to [sizeof] as variables, ignore those. *)
       if not (String.starts_with ~prefix:"sizeof(" v.name) then
         (** Look for [v] and its number of levels of indirection [nli] in the
             local [scope]. If [v] is not in the local scope, set [e] to [false]
             and suppose that [nli] is null, i.e. that [v] is a simple variable,
             not a pointer or a reference. *)
         let (nli, e) = Var_Hashtbl.find_or_default scope v 0 in
         (** If [v] is not in the local scope or if it is a global variable we
             know the definition of, i.e. if [v] is in [!Apac_records.globals],
             add [GlobalVariable] to the dependency attribute set [das] we are
             about to map to the dependency on [v]. *)
         let das =
           if (not e) || (Var_map.mem v !Apac_records.globals) then
             DepAttr_set.singleton GlobalVariable
           else
             DepAttr_set.empty in
         (** If the dependency appears within an index array operator, we must
             attribute it with [Accessor]. *)
         let das = if iao then DepAttr_set.add Accessor das else das in
         (** If [v] is not a pointer but a simple variable, a reference or an
             alias, *)
         if nli < 1 || alias then
           (** simply transform it into an adequate data dependency [d]. *)
           let d = Dep_var v in
           (** Finally, add [d] into either the set of input or the set of
               input-output dependencies according to its [access]
               classification and update [dam], the map of dependencies to sets
               of dependency attributes, if necessary, i.e. if [das] contains at
               least one new dependency attribute to go with [d]. *)
           let dam =
             if (DepAttr_set.is_empty das) then dam
             else Dep_map.add d das dam in
           match access with
           | `In -> (Dep_set.add d ins, inouts, dam) 
           | `InOut -> (ins, Dep_set.add d inouts, dam)
         else
           (** If [v] is a pointer, it may lead to multiple dependencies
               following the number of dereferencements, i.e. [gets]. In this
               case, we have to add a dependency on [v] as well as to each level
               of indirection of [v] between 0 and [gets].

               For example, let us consider a pointer variable [ptr] with 3
               levels of indirection. An access to [ptr] of the form [**ptr]
               generates the following dependencies ([ptr], [ptr\[0\]],
               [ptr\[0\]\[0\]]). *)
           begin
             (** However, if the [v] appears within a function call, we must
                 consider every single level of indirection of [v] as a
                 dependency, i.e. consider [nli] instead of [gets], as we do not
                 know at which level of indirection the function accesses [v].

                 For example, the access [**ptr] would lead to the following
                 dependencies ([ptr], [ptr\[0\]], [ptr\[0\]\[0\]],
                 [ptr\[0\]\[0\]\[0\]]). *)
             let degree = if call then nli else gets in
             let ds = Dep.of_degree t v degree in
             (** Processing of immutable variables *)
             if vk <> Var_immutable then
               begin
                 (** requires adding an extra get operation on [v] when
                     producing the output source code. *)
                 let t = trm_get t in
                 let ds' = Dep.of_degree t v degree in
                 (** To defer this operation until the code generation stage, we
                     use the [!Apac_records.mutables] map. *)
                 List.iter2 (fun pk pv ->
                     mutables := Dep_map.add pk pv !mutables
                   ) ds ds';
               end;
             (** At the end, we add the dependencies [ds] arising from [v] into
                 either the set of input or the set of input-output dependencies
                 according to their [access] classification. For each dependency
                 [d] in [ds], update also [dam], the map of dependencies to sets
                 of dependency attributes, if necessary, i.e. if [das] contains
                 at least one new dependency attribute to go with [d]. *)
             match access with
             | `In ->
                let ins, dam =
                  List.fold_left (fun (ins, dam) d ->
                      let dam =
                        if (DepAttr_set.is_empty das) then dam
                        else Dep_map.add d das dam in
                      (Dep_set.add d ins, dam)
                    ) (ins, dam) ds in
                (ins, inouts, dam)
             | `InOut ->
                let _, ins, inouts, dam =
                  List.fold_left (fun (i, ins, inouts, dam) d ->
                      let dam =
                        if (DepAttr_set.is_empty das) then dam
                        else Dep_map.add d das dam in
                      (** When [v] is an inout-dependency, we add to [inouts]
                          the dependencies on the level of indirection of [v]
                          appearing the original access term. The dependencies
                          on preceding levels of indirection belong to [ins].
                          However, when [v] appears with a function call, we
                          have to add to [inouts] the dependencies on all the
                          levels of indirection of [v] the function can alter by
                          side-effect.

                          Considering the example of [ptr] and the access
                          [**ptr], if the latter is an inout-dependency, we add
                          [ptr\[0\]\[0\]] to [inouts] and ([ptr], [ptr\[0\]]) to
                          [ins]. However, if the access appears within a
                          function call, e.g. [f(\*\*ptr)], we add
                          ([ptr\[0\]\[0\]], [ptr\[0\]\[0\]\[0\]]) to [inouts]
                          and ([ptr], [ptr\[0\]]) to [ins]. *)
                      if ((i > gets) && call) ||
                           ((not call) && (i >= gets)) then
                        (i + 1, ins, Dep_set.add d inouts, dam)
                      else
                        (i + 1, Dep_set.add d ins, inouts, dam)
                    ) (0, ins, inouts, dam) (List.rev ds) in
                (ins, inouts, dam)
           end
       else (ins, inouts, dam)
    (** [t] is a structure member access ([s->m], [s.m], ...). *)
    | Trm_apps ({ desc = Trm_val
                           (Val_prim (Prim_unop (Unop_struct_access _)));
                  _ }, _)
      | Trm_apps ({ desc = Trm_val
                             (Val_prim (Prim_unop (Unop_struct_get _)));
                    _ }, _) ->
       (** Our analysis is not clever enough yet. Try the best-effort dependency
           discovery, but warn the user about it. *)
       warning t; best_effort ins inouts dam access iao t
    (** [t] is a dereferencement ([\*ptr], [\*\*ptr], ...) of a term [t'']. *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [t'']) ->
       (** Simplify [\&\*t] and [\*\&t] in [t] to [t']. *)
       let t' = trm_simplify_addressof_and_get t in
       (** However, the simplification is not recursive. Continue until
           [!trm_simplify_addressof_and_get] has an effect on [t']. *)
       if t != t' then
         main ins inouts dam gets call access iao t'
       else
         (** Then, try to resolve the term [t'] of the variable being
             dereferenced as well as the number [d] of dereferencements. *)
         begin match (trm_resolve_dereferenced_with_degree t) with
         | Some (t', d) ->
            (** The encoding of OptiTrust implies an extra dereferencement when
                reading a variable, i.e. when [access] is [`In], or when the
                variable appears as an argument in a function call (see the
                [call] flag). Therefore, in these cases, we have to decrease [d]
                by one. *)
            let d' = if access = `InOut && not call then d else d - 1 in
            (** After ensuring a correct value of [d] in [d'], we continue the
                dependency discovery on the variable term [t']. *)
            main ins inouts dam d' call access iao t'
         (** In the case we are unable to resolve the term of the variable being
             dereferenced, we are facing two possible situations:

             - [t''] is a memory access consisting of more than a simple
             combination of dereferencements and array accesses the
             [!trm_resolve_dereferenced_with_degree] function understands. *)
         | None when is_access t'' ->
            (** If so, we continue the dependency discovery on [t'']. *)
            main ins inouts dam 0 call access iao t''
         (** - [t''] represents another kind of term. *)
         | None ->
            (** If so, we try the best-effort dependency discovery and warn the
                user about it. *)
            warning t; best_effort ins inouts dam access false t
         end
    (** [t] is a referencement ([\&ptr]) of a term [t']. *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address))}, [t']) ->
       (** Simplify [\&\*t] and [\*\&t] in [t] to [t']. *)
       let t'' = trm_simplify_addressof_and_get t in
       (** However, the simplification is not recursive. Continue until
           [!trm_simplify_addressof_and_get] has an effect on [t']. *)
       if t != t'' then
         main ins inouts dam 0 call access iao t''
       else if (trm_is_array_or_direct_access t') then
         (** If [t'] can resolve to an array or direct variable access, i.e.
             forms of data accesses our discovery algorithm understands, we
             continue the dependency discovery on [t']. *)
         main ins inouts dam 0 call access iao t'
       else
         (** Otherwise, we try the best-effort dependency discovery and warn the
             user about it. *)
         begin warning t; best_effort ins inouts dam access false t end
    (** [t] is an array accesses ([t\[i\]]). *)
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _) ->
       (** Retrieve the [base] variable term as well as the [accesses] within
           index array operators. *)
       let (base, accesses) = get_nested_accesses t in
       (** Due to OptiTrust encoding, an extra dereferencement may hide the
           variable term in [base]. *)
       let base =
         match base.desc with
         (** In such a case, we have to unwrap the [base] term and return the
             underlying variable term [vt]. *)
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [vt]) ->
            vt
         (** Otherwise, we keep the [base] term as-is. *)
         | _ -> base
       in
       begin match base.desc with
       (** When [base] represents a variable term, we can continue the
           dependency discovery process. *)
       | Trm_var (_, v) ->
          (** If [v] is an [alias] to an existing variable, make [v] become the
              latter. *)
          let v, alias = Var_Hashtbl.find_or_default aliases v v in
          (** Look for [v] and its number of levels of indirection [nli] in the
              local [scope]. If [v] is not in the local scope, set [e] to
              [false] and suppose that [nli] is null, i.e. that [v] is a simple
              variable, not a pointer or a reference. *)
          let (nli, e) = Var_Hashtbl.find_or_default scope v 0 in
          (** If [v] is not in the local scope or if it is a global variable we
              know the definition of, i.e. if [v] is in [!Apac_records.globals],
              add [GlobalVariable] to the dependency attribute set [das] we are
              about to map to the dependency on [v]. *)
          let das =
            if (not e) || (Var_map.mem v !Apac_records.globals) then
              DepAttr_set.singleton GlobalVariable
            else
              DepAttr_set.empty in
          (** If the dependency appears within an index array operator, we must
              attribute it with [Accessor]. *)
          let das = if iao then DepAttr_set.add Accessor das else das in
          (** If [v] is an alias, *)
          if alias then
            (** simply transform it into an adequate data dependency [d]. *)
            let d = Dep_var v in
            (** Finally, add [d] into either the set of input or the set of
                input-output dependencies according to its [access]
                classification and update [dam], the map of dependencies to sets
                of dependency attributes, if necessary, i.e. if [das] contains
                at least one new dependency attribute to go with [d]. *)
            let dam =
              if (DepAttr_set.is_empty das) then dam
              else Dep_map.add d das dam in
            match access with
            | `In -> (Dep_set.add d ins, inouts, dam) 
            | `InOut -> (ins, Dep_set.add d inouts, dam)
          else
            (** Count the number [c] of index array operator pairs involved in
                this array access. *)
            let c = List.length accesses in
            (** Transform [v] and the access term [t] into a dependency. As this
                is an array access, it leads to multiple dependencies following
                the number of dereferencements, i.e. [gets], and the number [c]
                of index array operator pairs, of course. In this case, we have
                to add a dependency on [v] and [t] as well as to each level of
                indirection of [v] and [t] between 0 and [gets + c].
                
                For example, let us consider an access [\*arr3D\[2\]] to a
                3-dimensional array [arr3D]. The latter has 3 levels of
                indirection and the access itself consists of a dereferencement
                of the first level of indirection using the [\*] operator and of
                a dereferencement of the second level of indirection using the
                index array operator. In this case, [c] is 1 and [gets] is 1
                too. With [!Dep.of_array] we generate the following dependencies
                ([arr3D], [\*arr3D], [\*arr3D\[2\]]).*)
            let ds = Dep.of_array t v in
            (** However, if the [t] appears within a function call and [v] is
                not fully dereferenced, i.e. the number of dereferencements
                [gets + c] is less than [nli], we must consider the remaining
                levels of indirection of [v] as dependencies as well, i.e.
                consider [nli] instead of [gets + c], as the function may access
                [v] through the remaining levels of indirection.
                
                For example, the access [\*arr3D\[2\]] would lead to the
                following dependencies ([arr3D], [\*arr3D], [\*arr3D\[2\]],
                [\*arr3D\[2\]\[0\]]). *)
            let rec complete = fun ds n b ->
              if n < b then complete ((Dep.of_trm t v 1) :: ds) (n + 1) b
              else ds in
            let ds =
              if call && (c + gets) < nli then complete ds 0 (nli - c - gets)
              else ds in
            (** At the end, we add the dependencies [ds] arising from [v] and
                [t] into either the set of input or the set of input-output
                dependencies according to their [access] classification. For
                each dependency [d] in [ds], update also [dam], the map of
                dependencies to sets of dependency attributes, if necessary,
                i.e. if [das] contains at least one new dependency attribute to
                go with [d]. *)
            let ins, inouts, dam =
              match access with
              | `In ->
                 let ins, dam =
                   List.fold_left (fun (ins, dam) d ->
                       let dam =
                         if (DepAttr_set.is_empty das) then dam
                         else Dep_map.add d das dam in
                       (Dep_set.add d ins, dam)
                     ) (ins, dam) ds in
                 (ins, inouts, dam)
              | `InOut ->
                 let _, ins, inouts, dam =
                   List.fold_left (fun (i, ins, inouts, dam) d ->
                       (** Add the [Subscripted] attribute to the dependencies
                           dereferencing [base] using index array operators.

                           Considering the example of [\*arr3D\[2\]], we have to
                           add the [Subscripted] attribute to [\*arr3D\[2\]] and
                           to [\*arr3D\[2\]\[0\]] if the access apears within a
                           function call. *)
                       let das =
                         if i > gets then DepAttr_set.add Subscripted das
                         else das in
                       let dam =
                         if (DepAttr_set.is_empty das) then dam
                         else Dep_map.add d das dam in
                       (** When [v] is an inout-dependency, we add to [inouts]
                           the dependencies on the level of indirection of [v]
                           appearing the original access term. The dependencies
                           on preceding levels of indirection belong to [ins].
                           However, when [v] appears with a function call, we
                           have to add to [inouts] the dependencies on all the
                           levels of indirection of [v] the function can alter
                           by side-effect.
                           
                           Considering the example of [\*arr3D\[2\]], if the
                           latter is an inout-dependency, we add [\*arr3D\[2\]]
                           to [inouts] and ([arr3D], [\*arr3D]) to [ins].
                           However, if the access appears within a function
                           call, e.g. [f(\*arr3D\[2\])], we add ([\*arr3D\[2\]],
                           [\*arr3D\[2\]\[0\]]) to [inouts] and ([arr3D],
                           [\*arr3D]) to [ins]. *)
                       if (call && (i > c)) ||
                            (call && (c + gets) >= nli && (i >= c))  ||
                              ((not call) && (i >= c)) then
                         (i + 1, ins, Dep_set.add d inouts, dam)
                       else
                         (i + 1, Dep_set.add d ins, inouts, dam)
                     ) (0, ins, inouts, dam) (List.rev ds) in
                 (ins, inouts, dam)
            in
            (** At the end, we must look for data dependencies among the
                [accesses] within index array operators.*)
            List.fold_left (fun (ins, inouts, dam) a ->
                match a with
                | Array_access_get t
                  | Array_access_addr t ->
                   main ins inouts dam 0 false `In true t
                | _ -> (ins, inouts, dam)
              ) (ins, inouts, dam) accesses
       (** When [base] does not represent a variable term, it is not an access
           pattern our dependency discovery fully recognizes so we try the
           best-effort approach and warn the user about it. *)
       | _ -> warning t; best_effort ins inouts dam access false t
       end
    (** [t] is a unary increment or decrement operation [op] ([t++], [--t]) with
        the operand [t']. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) when
           (is_prefix_unary op || is_postfix_unary op) ->
       (** Due to OptiTrust encoding, an extra dereferencement may hide the
           variable term [vt] in the operand [t']. *)
       let t' =
         match t'.desc with
         (** In such a case, we have to unwrap the [t'] term and return the
             underlying variable term [vt]. *)
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [vt]) ->
            vt
         (** Otherwise, we keep the [t'] term as-is. *)
         | _ -> t'
       in
       begin match t'.desc with
       (** When [t'] represents a variable term, we can continue the dependency
           discovery process. *)
       | Trm_var _ -> main ins inouts dam 0 call `InOut iao t'
       (** Otherwise, it is not an access pattern we fully recognize so we try
           the best-effort approach and warn the user about it. *)
       | _ -> warning t'; best_effort ins inouts dam `InOut iao t'
       end
    (** [t] is a call ([f(arg0, arg1, ...)]) to a function [f] with [args]. *)
    | Trm_apps ({ desc = Trm_var (_ , f); _ }, args) ->
       (** If we have [f] on the record in [!Apac_records.functions], *)
       if Var_Hashtbl.mem functions f then
         (** we retrieve the corresponding record [r] while *)
         let r : FunctionRecord.t = Var_Hashtbl.find functions f in
         (** skipping [this] in [args], if any, and *)
         let args = if f.name = "this" then List.tl args else args in
         (** continue the dependency discovery on each argument [arg] in [args].
             Here, each corresponding argument record [ar] in [r.args] tell us
             if an [arg] is read or read-write (see [!type:FunctionRecord.a]).
             This way, we can analyze each [arg] while passing the correct
             access classification to [!discover_dependencies.main] through its
             [access] enumeration parameter. *)
         List.fold_left2 (fun (ins, inouts, dam) arg (ar, _) ->
             let access = if (FunctionRecord.is_rw ar) then `InOut else `In in
             main ins inouts dam 0 true access iao arg
           ) (ins, inouts, dam) args r.args
       else
         (** If we do not have [f] on the record, we cannot know which of the
             arguments [f] is likely to modify by side-effect. We thus safely
             consider each argument [arg] in [args] as a potential input-output
             dependency. *)
         List.fold_left (fun (ins, inouts, dam) arg ->
             main ins inouts dam 0 true `InOut iao arg
           ) (ins, inouts, dam) args
    (** [t] is a set operation ([a = 1]) between an [lval] and an [rval]. *)
    | Trm_apps _ when is_set_operation t ->
       (** Deconstruct the set operation term [t] into [lval] and [rval]. *)
       let error' =
         "Apac_task_candidate_discovery.discover_dependencies.main: expected \
          set operation." in
       let (lval, rval) = trm_inv ~error:error' set_inv t in
       (** Check whether we can resolve the variable [v] behind [lval]. If so,
           we check whether this variable declaration introduces an alias to an
           existing variable in which case we update [aliases], then we continue
           the dependency discovery in [lval] and [rval]. On the one hand,
           [lval] is the memory area being modified, we thus classify the
           dependency as an inout-dependency. On the other hand, [rval] is being
           read assigned to [lval], we thus consider it as an in-dependency.
           However, this might change later if we discover a unary increment or
           decrement in [rval]. *)
       begin
         match trm_resolve_binop_lval_and_get_with_deref ~plus:true lval with
         | Some ({ v; _}, d) ->
            may_update_aliases (not d) v (Typ.typ_unit ()) rval;
            let ins, inouts, dam =
              main ins inouts dam 0 false `InOut false lval in
            main ins inouts dam 0 false `In false rval
         | None -> fail t.loc error
       end
    (** [t] is a single declaration ([int a;], [int \* b = ptr;]) of a variable
        [v] of type [ty] and kind [vk] optionally involving an initialization
        term [init]. *)
    | Trm_let (vk, (v, ty), init, _) ->
       (** Determine the number of levels of indirection [nli] of [v] thanks to
           its type [ty]. *)
       let nli = typ_get_nli ty in
       (** Mutable variables in OptiTruts feature an addition dereferencement
           operation we must not take into account here. This appears to be
           necessary only in the case of single variable declarations. *)
       let nli = if vk = Var_immutable then nli else nli - 1 in
       (** Transform [v] into an inout-dependency and add it to the
           corresponding dependency set. *)
       let inouts = Dep_set.add (Dep_var v) inouts in
       (** We continue the dependency discovery within the initialization term
           [init]. At this point, we consider them as in-dependencies. However,
           this might change later if we discover a unary increment or decrement
           in [init]. *)
       let ins, inouts, dam = main ins inouts dam 0 false `In false init in
       (** [v] also represents a new variable in the local [scope], we thus need
           to introduce it to the latter together with its [nli]. *)
       Var_Hashtbl.add scope v nli;
       (** Finally, we check whether this variable declaration introduces an
           alias to an existing variable and if so, we update [aliases]. *)
       may_update_aliases (trm_has_cstyle Reference t) v ty init;
       (ins, inouts, dam)
    (** [t] is a multiple declaration ([int a = 1, b]) of variables [tvs]
        including their types and of kind [vk] optionally involving
        initialization terms [inits]. *)
    | Trm_let_mult (vk, tvs, inits) ->
       (** For each variable [v] of type [ty] optionally involving an
           initialization term [init], *)
       List.fold_left2 (fun (ins, inouts, dam) (v, ty) init ->
           (** Determine the number of levels of indirection [nli] of [v] thanks
               to its type [ty]. *)
           let nli = typ_get_nli ty in
           (** Transform [v] into an inout-dependency and add it to the
               corresponding dependency set. *)
           let inouts = Dep_set.add (Dep_var v) inouts in
           (** We continue the dependency discovery within the initialization
               term [init]. At this point, we consider them as in-dependencies.
               However, this might change later if we discover a unary increment
               or decrement in [init]. *)
           let ins, inouts, dam = main ins inouts dam 0 false `In false init in
           (** [v] also represents a new variable in the local [scope], we thus
               need to introduce it to the latter together with its [nli]. *)
           Var_Hashtbl.add scope v nli;
           (** Finally, we check whether this variable declaration introduces an
               alias to an existing variable and if so, we update [aliases]. *)
           may_update_aliases (Typ.is_reference ty) v ty init;
           (ins, inouts, dam)
         ) (ins, inouts, dam) tvs inits
    (** [t] is none of the above, explore the child terms. *)
    | Trm_apps (f, args) ->
       List.fold_left (fun (ins, inouts, dam) item ->
           main ins inouts dam 0 call `In false item
         ) (ins, inouts, dam) (f :: args)
    | Trm_array { items; _ } ->
       List.fold_left (fun (ins, inouts, dam) item ->
           main ins inouts dam 0 call `In false item
         ) (ins, inouts, dam) items
    | Trm_record { items; _ } ->
       List.fold_left (fun (ins, inouts, dam) (_, record) ->
           main ins inouts dam 0 call `In false record
         ) (ins, inouts, dam) items
    | Trm_abort (Ret (Some return)) ->
       main ins inouts dam 0 false access false return
    (** This function cannot explore any other term, return. *)
    | _ -> (ins, inouts, dam)
  in
  (** Launch the dependency discovery process with default parameters. *)
  main (Dep_set.empty) (Dep_set.empty) (Dep_map.empty) 0 false `In false t

(* [taskify_on p t]: see [taskify]. *)
let taskify_on (p : path) (t : trm) : unit =
  (* Auxiliary function to transform a portion of the existing AST into a local
     fill_task_graphed AST (see [atrm]). *)
  let rec fill (s : FunctionRecord.s) (a : var Var_Hashtbl.t)
            (t : trm) (g : TaskGraph.t) : Task.t =
    match t.desc with
    | Trm_seq sequence ->
       (** Keep a copy of the local scope of variables as a map. We need this
           because we do not want any variables potentially defined in child
           scopes (added to [s] within [trm_discover_dependencies]) to end up in
           the dependency sets of the parent scope, which is the current
           scope. *)
       let scope = var_map_of_var_hashtbl s in
       (** Convert the marked list of statements of the [sequence] into a simple
           list of statements. *)
       let instrs = Mlist.to_list sequence in
       (** Transform the statements of the [sequence] into task candidates
           within the task candidate graph [g]. *)
       let tasks = List.map (fun instr -> fill s a instr g) instrs in
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
       (** Add the task candidates representing the statements of the [sequence]
           into the task candidate graph [g] while assigning them a logical
           schedule. *)
       let tasks = List.map (fun (task : Task.t) ->
                       task.schedule <- schedule ();
                       let v = TaskGraph.V.create task in
                       TaskGraph.add_vertex g v; v) tasks in
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
       let scope = var_map_of_var_hashtbl s in
       (* Launch dependency discovery in the initialization term as well as *)
       let (ins, inouts, ioattrs) = discover_dependencies s a init in
       (* in the conditional statement representing the upper loop bound. *)
       let (ins', inouts', ioattrs') = discover_dependencies s a cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton Condition) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton Condition) ioattrs' in
       (* Gather the discovered dependencies and attributes. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs') in
       (* Launch dependency discovery in the increment term. *)
       let (ins', inouts', ioattrs') = discover_dependencies s a inc in
       (* Add the [InductionVariable] attribute to the input and input-output
          dependencies discovered in the increment term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       (* Gather the discovered dependencies and attributes. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs') in
       (* Create a sub-graph for the body sequence, i.e. [instr], of the
          for-loop. *)
       let c = TaskGraph.create() in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let ct = fill s a instr c in
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
       let tas = TaskAttr_set.singleton Singleton in
       (* Create the task corresponding to the current for-node using all the
          elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[c]]
    | Trm_for (range, instr, _) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the dependency sets of the parent scope, which is the current
          scope. *)
       let scope = var_map_of_var_hashtbl s in
       (* Explode the [range] specifier to allow for dependency discovery. *)
       let (index, init, _, cond, step, _) = range in
       (* Launch dependency discovery in the initialization term as well as *)
       let (ins, inouts, ioattrs) = discover_dependencies s a init in
       (** Add the iterator [index] to the scope of the current loop. *)
       Var_Hashtbl.add s index 0;
       (* in the conditional statement representing the upper loop bound. *)
       let (ins', inouts', ioattrs') = discover_dependencies s a cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton Condition) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton Condition) ioattrs' in
       (* Gather the discovered dependencies and attributes. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs') in
       (* Check whether [step] is formed of a term. In other words, check
          whether it is not simply a unary increment or decrement, but something
          like [i += a * 2]. In this case, *)
       let (ins', inouts', ioattrs') = match step with
         (* we have to look for dependencies in this term. *)
         | Step st -> discover_dependencies s a st
         (* Otherwise, we have to add an input-output dependency on the
            induction variable [index]. *)
         | _ ->
            let div = Dep_var index in
            (Dep_set.empty, Dep_set.singleton div, Dep_map.empty)
       in
       (* Add the [InductionVariable] attribute to the input and input-output
          dependencies discovered in the increment term of the for-loop. *)
       let ioattrs' = Dep_map.bind_set ins'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       let ioattrs' = Dep_map.bind_set inouts'
                        (DepAttr_set.singleton InductionVariable) ioattrs' in
       (* Gather the discovered dependencies, if any. *)
       let (ins, inouts, ioattrs) =
         (Dep_set.union ins ins',
          Dep_set.union inouts inouts',
          Dep_map.union2 ioattrs ioattrs') in
       (* Create a sub-graph for the body sequence, i.e. [instr], of the
          for-loop. *)
       let c = TaskGraph.create() in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let ct = fill s a instr c in
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
       let tas = TaskAttr_set.singleton Singleton in
       (* Create the task corresponding to the current for-node using all the
          elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[c]] 
    | Trm_let _
      | Trm_let_mult _ ->
       (* Look for dependencies in the current variable declaration term and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts, ioattrs) = discover_dependencies s a t in
       (* Convert the updated local scope to a set. *)
       let scope' = var_map_of_var_hashtbl s in
       (* Variable declarations should never become tasks, but rather
          synchronization barriers, nor be merged with other task graph
          nodes. *)
       let tas = TaskAttr_set.singleton Singleton in
       (* Create a barrier corresponding to the current variable declaration
          term. Variable declarations should never appear in tasks. *)
       Task.create (-1) t tas scope' ins inouts ioattrs [[]]
    | Trm_apps _ ->
       (** Is [t] an assignment to [Apac_macros.result_variable] we introduce in
           the 'return' replacement [Apac_prologue.use_goto_for_return]? *)
       let isjump = if (is_set_operation t) then
                      let error =
                        "Apac_task_candidate_discovery.taskify_on.fill: \
                         expected set operation." in
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
         Task.create (-1) t attrs Var_map.empty
           Dep_set.empty Dep_set.empty Dep_map.empty []
       else
         (** Look for dependencies and their attributes in the current term and
             initialize the in and inout-dependency sets as well as the map of
             dependency attribute sets. *)
         let (ins, inouts, ioattrs) = discover_dependencies s a t in
         (** Convert the local scope to a set. *)
         let scope = var_map_of_var_hashtbl s in
         (** Create the corresponding task candidate using all the elements
             computed above. *)
         Task.create (-1) t (TaskAttr_set.empty) scope ins inouts ioattrs [[]]
    | Trm_if (cond, yes, no) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_map_of_var_hashtbl s in
       (* Look for dependencies and their attributes in the conditional
          expression of the [if] and initialize the in and in-out dependency
          sets as well as the map of dependency attribute sets. *)
       let (ins, inouts, ioattrs) = discover_dependencies s a cond in
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
       let ty = fill s a yes gy in
       (* If there is no [else] branch, create an empty task. *)
       let missing_tn = is_trm_unit no in 
       let tn = if missing_tn then Task.empty () else fill s a no gn in
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
       let tas = TaskAttr_set.singleton Singleton in
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
       let scope = var_map_of_var_hashtbl s in
       (* Look for dependencies and their attributes in the conditional
          expression of the [while] and initialize the in and in-out dependency
          sets as well as the map of dependency attributes. *)
       let (ins, inouts, ioattrs) = discover_dependencies s a cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the while-loop. *)
       let ioattrs = Dep_map.bind_set ins
                       (DepAttr_set.singleton Condition) ioattrs in
       let ioattrs = Dep_map.bind_set inouts
                       (DepAttr_set.singleton Condition) ioattrs in
       (* Create a sub-graph for the body sequence of the [while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s a body gb in
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
       let tas = TaskAttr_set.singleton Singleton in
       (* Create the task corresponding to the current [while] graph node using
          all the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[gb]]
    | Trm_do_while (body, cond) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_map_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the do-while
          and initialize the in and in-out dependency sets as well as the map of
          dependency attribute sets. *)
       let (ins, inouts, ioattrs) = discover_dependencies s a cond in
       (* Add the [Condition] attribute to the input and input-output
          dependencies discovered in the condition term of the do-while-loop. *)
       let ioattrs = Dep_map.bind_set ins
                       (DepAttr_set.singleton Condition) ioattrs in
       let ioattrs = Dep_map.bind_set inouts
                       (DepAttr_set.singleton Condition) ioattrs in
       (* Create a sub-graph for the body sequence of the [do-while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s a body gb in
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
       let tas = TaskAttr_set.singleton Singleton in
       (* Create the task corresponding to the current [do-while] graph node
          using all the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [[gb]]
    | Trm_switch (cond, cases) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_map_of_var_hashtbl s in
       (* Look for dependencies and their attributes in the conditional
          expression of the [switch] and initialize the in and in-out dependency
          sets as well as the map of dependency attribute sets. *)
       let (ins, inouts, ioattrs) = discover_dependencies s a cond in
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
           let tb = fill s a block gb in
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
       let tas = TaskAttr_set.singleton Singleton in
       (* Create the task corresponding to the current [switch] graph node
          using all the elements computed above. *)
       Task.create (-1) t tas scope ins inouts ioattrs [gbs]
    | Trm_delete (_, target) ->
       (* Look for dependencies in the target term of the [delete]. [delete] is
          a destructive operation, we need to consider all of the dependencies
          as in-out dependencies, of course. *)
       let (ins, inouts, ioattrs) = discover_dependencies s a target in
       let inouts = Dep_set.union ins inouts in
       (* Convert the local scope to a set *)
       let scope = var_map_of_var_hashtbl s in
       (* Transform this task into a synchronization barrier. *)
       let tas = TaskAttr_set.singleton WaitForSome in
       (* in order to be able to use it when creating the task corresponding to
          the current [delete] graph node. *)
       Task.create (-1) t tas scope Dep_set.empty inouts Dep_map.empty [[]]
    | Trm_goto target ->
       (** If the target label of the 'goto' is not the [Apac_core.goto_label]
           we use within the return statement replacement transformation
           [Apac_basic.use_goto_for_return], fail. We do not allow for other
           'goto' statements than [Apac_core.goto_label]. *)
       if target <> Apac_macros.goto_label then
         fail t.loc "Apac_task_candidate_discovery.taskify_on.fill: illegal \
                     'goto' statement."
       else
         (** If [target] is [Apac_core.goto_label], we can transform it into a
             task candidate carrying the [IsJump] attribute to indicate the
             presence of the 'goto' statement. *)
         let attrs = TaskAttr_set.singleton IsJump in
         Task.create (-1) t attrs Var_map.empty
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
          Task.create (-1) t attrs Var_map.empty
            Dep_set.empty Dep_set.empty Dep_map.empty [[]]
       (** Otherwise, fail. We do not allow for other types of values in
           first-level instructions. *)
       | _ ->
          let it = AstC_to_c.ast_to_string t in
          let error =
            Printf.sprintf
              "Apac_task_candidate_discovery.taskify_on.fill: illegal value \
               term '%s'." it in
          fail t.loc error
       end
    | Trm_omp_routine r ->
       (* Convert the local scope to a set. *)
       let scope = var_map_of_var_hashtbl s in
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
          let (ins, inouts, ioattrs) = discover_dependencies s a (trm_var v) in
          let inouts = Dep_set.union ins inouts in
          (* Create the task corresponding to the current OpenMP routine call
             graph node. *)
          Task.create (-1) t attrs scope Dep_set.empty inouts ioattrs [[]]
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
                     "Apac_task_candidate_discovery.taskify_on.fill: \
                      statements of type `%s' should not appear in a task \
                      group."
                     (trm_desc_to_string t.desc) in
       fail t.loc error
  in
  (** Find the parent function [f]. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_task_candidate_discovery.taskify_on: unable to \
                          find parent function. Task group outside of a \
                          function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find functions f in
  (** Verify that the task candidate graph representation of [f] does not exist
      yet. Indeed, we are about to build it. *)
  if not (TaskGraph.is_empty r.graph) then
    let error = Printf.sprintf "Apac_task_candidate_discovery.taskify_on: task \
                                candidate graph of '%s' should not exist yet."
                  f.name in
    fail t.loc error
  else
    (** Initialize a hash table of variables for storing aliasing relationships
        ([!type:var] -> [!type:var]) during the dependency discovery. *)
    let aliases : var Var_Hashtbl.t = Var_Hashtbl.create 99 in
    (** Translate the function definition into task candidate graph intermediate
        representation and store the latter in [r]. *)
    let _ = fill r.scope aliases t r.graph in
    (** Optimize the edges of the graph thanks to transitive reduction. *)
    r.graph <- TaskGraphOper.recursive_transitive_reduction r.graph;
    (** Dump the output task candidate graph, if requested. *)
    if !Apac_flags.verbose then
      begin
        Printf.printf "Task candidate graph of `%s':\n" (var_to_string f);
        TaskGraphPrinter.print r.graph
      end;
    if !Apac_flags.keep_graphs then
      TaskGraphExport.to_pdf r.graph (Apac_macros.gf f)

(** [taskify tg]: expects the target [tg] to point at a function body. It then
    translates its abstract syntax tree representation into a task candidate
    graph representation. *)
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
  (** Find the parent function [f]. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_task_candidate_discovery.merge_on: unable to \
                          find parent function. Task group outside of a \
                          function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find functions f in
  (** Apply the merge on the task candidate graph of [f] in [r]. *)
  one r.graph;
  (** Dump the resulting task candidate graph, if requested. *)
  if !Apac_flags.verbose then
    begin
      Printf.printf "Task candidate graph of `%s' (merge):\n" (var_to_string f);
      TaskGraphPrinter.print r.graph
    end;
  if !Apac_flags.keep_graphs then
    TaskGraphExport.to_pdf r.graph (Apac_macros.gf ~suffix:"merge" f)

(** [merge tg]: expects the target [tg] to point at a function body. It then
    tries to optimize its task candidate graph representation by merging
    inter-dependent task candidates that could not yield parallelizable tasks.
    The idea is to identify and squash sequences of inter-dependent task
    candidates beginning with a task candidate having a single successor,
    containing task candidates with a single predecessor and a single successor,
    to end with a task candidate having a single predecessor.

    Let us take the example of the following task candidate graph with 5
    vertices ([{v1}] to [{v5}]):

    {[+-----------------------+     +-----------------------------+
      |         {v3}          |     |            {v1}             |
      |    c = add(c, b);     |     | int a = 10, b = 11, c = 10; |
      | (in: [b], inout: [c]) | <-- | (in: [], inout: [a, b, c])  |
      +-----------------------+     +-----------------------------+
        |                             |
        |                             |
        v                             v
      +-----------------------+     +-----------------------------+
      |         {v5}          |     |            {v2}             |  
      |    c = mul(c, b);     |     |       a = add(a, b);        |
      | (in: [b], inout: [c]) |     |    (in: [b], inout: [a])    |
      +-----------------------+     +-----------------------------+
                                      |
                                      |
                                      v
                                    +-----------------------------+
                                    |            {v4}             |  
                                    |       a = mul(a, b);        |
                                    |    (in: [b], inout: [a])    |
                                    +-----------------------------+]}

    There are two sequences of mergeable vertices in the above graph, i.e. the
    sequence containing [{v2}] and [{v4}] as well as the sequence containing
    [{v3}] and [{v5}]. The application of the [merge] transformation on the
    above task candidate graph would result in this task candidate graph with 3
    vertices:

    {[+-----------------------+     +-----------------------------+
      |         {v3}          |     |            {v1}             |
      |    c = add(c, b);     |     | int a = 10, b = 11, c = 10; |
      |    c = mul(c, b);     |     | (in: [], inout: [a, b, c])  |
      | (in: [b], inout: [c]) | <-- |                             |
      +-----------------------+     +-----------------------------+
                                      |
                                      |
                                      v
                                    +-----------------------------+
                                    |            {v2}             |
                                    |       a = add(a, b);        |
                                    |       a = mul(a, b);        |
                                    |    (in: [b], inout: [a])    |
                                    +-----------------------------+]}

    Indeed, the transformation merged the vertices [{v2}] and [{v4}] into a
    single vertex [{v2}] as well as the vertices [{v3}] and [{v5}] into a single
    vertex [{v3}]. *)
let merge (tg : target) : unit =
  Nobrace.enter ();
  Target.iter (fun t p -> merge_on p (get_trm_at_path p t)) tg

(** [detect_tasks_simple_on p t]: see [detect_tasks_simple_on]. *)
let detect_tasks_simple_on (p : path) (t : trm) : unit =
  (** [detect_tasks_simple_on.aux v]: if the vertex [v] consists in a call to a
      function we know the definition of without featuring child scopes or
      references to global variables, attribute it [Taskifiable]. If [v]
      involves nested candidate graphs, process them recursively. *)
  let rec aux (v : TaskGraph.V.t) : unit =
    let t = TaskGraph.V.label v in
    (** Initialize a counter of function calls [k]. *)
    let k = ref 0 in
    (** Loop over the abstract syntax tree representations [c] of [v] and *)
    let rec loop = fun c ->
      match c.desc with
      (** and if [c] is a call to a function [f] we know the definition of, i.e.
          if [f] is in [Apac_records.functions], increment [k] and continue
          exploring the abstract syntax tree. *)
      | Trm_apps ({ desc = Trm_var (_, f)}, _)
           when Var_Hashtbl.mem functions f ->
         incr k;
         trm_iter loop c
      (** When [c] implies a child scope, do nothing and stop exploring the
          abstract syntax tree. *)
      | Trm_seq _
        | Trm_for _
        | Trm_for_c _
        | Trm_if _
        | Trm_while _
        | Trm_do_while _
        | Trm_switch _ -> ()
      (** Otherwise, simply continue exploring the abstract syntax tree. *)
      | _ -> trm_iter loop c
    in
    List.iter (fun e -> loop e) t.current;
    (** Mark the task candidate as [Taskifiable] if there is at least one call
        to a function [f] we know the definition of. *)
    if !k > 0 then
      t.attrs <- TaskAttr_set.add Taskifiable t.attrs;
    (** When [v] features nested candidate graphs, explore the substatements. *)
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter aux go
          ) gl
      ) t.children
  in
  (** Find the parent function [f]. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc
                "Apac_task_candidate_discovery.detect_tasks_simple_on: unable \
                 to find parent function. Task group outside of a function?" in
  (** Find its function record [r] in [!Apac_records.functions]. *)
  let r = Var_Hashtbl.find functions f in
  (** Add the [Taskifiable] attribute to every task candidate featuring a call
      to a function we know the definition of. *)
  TaskGraphTraverse.iter aux r.graph;
  (** Dump the resulting task candidate graph, if requested. *)
  if !Apac_flags.verbose then
    begin
      Printf.printf "Task candidate graph of `%s' (detection):\n"
        (var_to_string f);
      TaskGraphPrinter.print r.graph
    end;
  if !Apac_flags.keep_graphs then
    TaskGraphExport.to_pdf r.graph (Apac_macros.gf ~suffix:"detection" f)

(** [detect_tasks_simple tg]: expects the target [tg] to point at a function
    body. It then scans its task candidate graph representation for eligible
    task candidates. This function applies a simple strategy which consists in
    adding the [Taskifiable] attribute to every task candidate featuring a call
    to a function we know the definition of, i.e. a function with a function
    definition record in [!Apac_records.functions]. *)
let detect_tasks_simple (tg : target) : unit =
  Target.iter (fun t p -> detect_tasks_simple_on p (get_trm_at_path p t)) tg

