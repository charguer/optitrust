open Ast
open Ast_to_c
open Target
open Path_constructors
open Transformations

(*
  find the definition x = dx pointed at by pl and replace occurrences of dx with
  x
  paths point at subterms in which all occurences will be replaced
  the empty path means all occurences will be replaced (default behaviour)
  as_reference option for variable declarations: if dx = &dx' replace dx' with
  *x instead of &dx' with x
 *)
let fold_decl (clog : out_channel) ?(as_reference : bool = false)
  ?(fold_at : target list = [[]]) (tr : target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [dl] ->
     let (t_def, _) = resolve_path dl t in
     let log : string =
       Printf.sprintf
         ("  - expression\n%s\n" ^^
          if as_reference then
          "    is a variable declaration of the form\n" ^^
          "      type* x = &dx\n"
          else
          "    is a variable/type declaration\n"
         )
         (ast_to_string t_def)
     in
     write_log clog log;
     begin match t_def.desc with
     (* const variables *)
     (* | Trm_decl (Def_var ((x, _), dx)) -> *)
      | Trm_let (_,(x,_),dx) ->
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
          else trm_var x
        in
        let def_x =
          if not as_reference then dx
          else
            match dx.add with
            | Add_address_of_operator :: addl -> {dx with add = addl}
            | _ -> fail t_def.loc "fold_decl: expected a reference"
        in
        let t = change_trm ~change_at:fold_at def_x t_x t in
        (*
          def_x might have been replaced with x in the definition of x
          -> replace it again with def_x
         *)
        let change_at =
         (* TODO: Fix later this temporary hack *)
          [[cVarDef x ~body:[cVar x ]; cBody]]
        in
        change_trm ~change_at t_x def_x t
     (*
       heap allocated variables
       note: an initialisation must be given
      *)
      (* TODO: Remove this *)
     (* | Trm_seq [{desc = Trm_decl (Def_var ((x, _), _)); _};
                {desc = Trm_apps (_, [_; dx]); _}]
          when t_def.annot = Some Heap_allocated ->
        let t_x =
          trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var x]
        in
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [t_x] else t_x
        in
        let def_x =
          if not as_reference then dx
          else
            match dx.add with
            | Add_address_of_operator :: addl -> {dx with add = addl}
            | _ -> fail t_def.loc "fold_decl: expected a reference"
        in
        let t = change_trm ~change_at:fold_at def_x t_x t in
        (* make sure def_x is not replaced in the definition of x here too *)
        let change_at =
          [[cVarDef x ~body:[cVar x]; cNth 1;
            cArg 1]]
        in
        change_trm ~change_at t_x def_x t *)
     (* typedef *)
     | Trm_decl (Def_typ (x, dx)) ->
        let ty_x = typ_var x in
        let t = change_typ ~change_at:fold_at dx ty_x t in
        (* make sure dx is not replaced in the definition of x here too *)
        let change_at = [[cTypDef x]] in
        change_typ ~change_at ty_x dx t
     (* fun decl *)
     | Trm_decl (Def_fun _) ->
        fail t.loc "fold_decl: fun declaration folding is unsupported"
     | _ -> fail t.loc "fold_decl: expected a definition"
     end
  | _ -> fail t.loc "fold_decl: the path must point to exactly 1 subterm"

(*
  insert a definition x = dx either before the position pointed at by
  insert_before or after the position pointed at by insert_after
  both must be resolved as paths to a seq element
  x may be a const variable or not (not const by default)
  option: make x a reference (x = &dx)
  assumptions:
    - no conflicts with the new name x
    - for a given seq, the insertion path points to at most one of its elements
    - if x is a reference, dx denotes a memory cell
 *)
let insert_decl ?(insert_before : target = [])
  ?(insert_after : target = []) ?(const : bool = false)
  ?(as_reference : bool = false) (x : var) (dx : trm) (t : trm) : trm =
  let tx =
    match dx.typ with
    | None -> fail dx.loc "insert_decl: cannot find definition type"
    | Some tx -> if as_reference then typ_ptr tx else tx
  in
  let def_x =
    if as_reference then {dx with add = Add_address_of_operator :: dx.add}
    else dx
  in
  let t_insert =
    if const then trm_let Var_immutable (x,tx) def_x
    (* if const then trm_decl (Def_var ((x, tx), def_x)) *)
    else
      trm_let Var_heap_allocated (x, (typ_ptr tx)) def_x
      (* trm_seq ~annot:(Some Heap_allocated)
        [trm_decl (Def_var ((x, typ_ptr tx), trm_prim (Prim_new tx)));
         trm_set ~annot:(Some Initialisation_instruction) (trm_var x) def_x
        ] *)
  in
  (* compute the explicit path for later use *)
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> insert_after
    | _ :: _, [] -> insert_before
    | [], [] -> fail t.loc "insert_decl: please specify an insertion point"
    | _ -> fail t.loc "insert_decl: cannot insert both before and after"
  in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target p t in
  Flags.verbose := b;
  (* insert the definition *)
  let t = insert_trm ~insert_before ~insert_after t_insert t in
  (*
    don't forget the delete instruction if x is heap allocated
    use explicit path because p will not be resolved as the position of the
    definition
   *)
  if const then t
  else
    (*
      add a seq with delete instruction around the pointed term containing the
      declaration
     *)
    let create_delete_instr (dl : path) (t : trm) : trm =
      apply_local_transformation
        (fun t ->
          (* t is expected to be a seq *)
          trm_seq (* ~annot:(Some Delete_instructions) *)
            [t;
             (* trm_apps ~annot:(Some Heap_allocated)
               ~typ:(Some (typ_unit ()))
               (trm_unop (Unop_delete false)) [trm_var x] *)
            ]
        )
        t
        dl
    in
    List.fold_left
      (fun t dl ->
        match List.rev dl with
        (*
          the seq containing the definition might be inside a seq with delete
          instructions
          -> do not create a seq
         *)
        (* | Dir_nth _ :: Dir_nth n :: dl ->
           apply_local_transformation
             (fun t ->
               match t.desc with
               | Trm_seq (t' :: del_instr_l)
                    when t.annot = Some Delete_instructions ->
                  trm_seq ~annot:(Some Delete_instructions)
                    (t' ::
                     (trm_apps ~annot:(Some Heap_allocated)
                        ~typ:(Some (typ_unit ()))
                        (trm_unop (Unop_delete false)) [trm_var x]) ::
                     del_instr_l
                    )
               (*
                 if we do not find a seq of delete instructions, go deeper to
                 create the seq
                *)
               | _ -> create_delete_instr [Dir_nth n] t
             )
             t
             (List.rev dl) *)
        | Dir_nth _ :: dl -> create_delete_instr (List.rev dl) t
        | _ -> fail t.loc "insert_definition: expected a path to a seq"
      )
      t
      epl

(* same as insert_definition but for a constant *)
let insert_const ?(insert_before : target = [])
  ?(insert_after : target = []) (x : var) (dx : trm) (t : trm) : trm =
  insert_decl ~insert_before ~insert_after ~const:true x dx t

(*
  insert a type declaration x = dx either before the position pointed at by
  insert_before or after the position pointed at by insert_after
  both must be resolved as paths to a seq element
  assumption: no conflicts with the new name x
 *)
let insert_typedef ?(insert_before : target = [])
  ?(insert_after : target = []) (x : typvar) (dx : typ) (t : trm) : trm =
  insert_trm ~insert_before ~insert_after (trm_decl (Def_typ (x, dx))) t

(*
  combine insert_definition and fold_decl
  assumption: if x is not a reference, no effects for dx and it has the same
  value through all its occurences
 *)
let insert_and_fold (clog : out_channel) ?(insert_before : target = [])
  ?(insert_after : target = []) ?(const : bool = false)
  ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (x : var)
  (dx : trm) (t : trm) : trm =
  (* compute the explicit path for later use *)
  let p =
    match insert_before, insert_after with
    | [], _ :: _ ->  insert_after
    | _ :: _, [] ->  insert_before
    | [], [] -> fail t.loc "insert_and_fold: please specify an insertion point"
    | _ -> fail t.loc "insert_and_fold: cannot insert both before and after"
  in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target p t in
  Flags.verbose := b;
  (* insert the definition *)
  let t =
    insert_decl ~insert_before ~insert_after ~const ~as_reference x dx t
  in
  (*
    fold the definition
    use explicit path because p will not be resolved as the position of the
    definition
    any path in epl is ok to do so
   *)
  match epl with
  | [] -> fail t.loc "insert_and_fold: no insertion point"
  | dl :: _ ->
     let def_pathl =
       let pathl_of_expl_path (dl : path) : target =
         List.map (fun d -> Constr_dir d) dl
       in
       match List.rev dl with
       | Dir_nth n :: dl ->
          let n =
            match insert_before, insert_after with
            (* insert after: add 1 to n *)
            | [], _ :: _ -> n + 1
            (* insert before: n is the position of the definition *)
            | _ :: _, [] -> n
            | [], [] ->
               fail t.loc "insert_and_fold: please specify an insertion point"
            | _ ->
               fail t.loc "insert_and_fold: cannot insert both before and after"
          in
          let (t_container, _) = resolve_path (List.rev dl) t in
          begin match t_container.annot with
          (*
            in case of heap allocation, a seq (for delete instructions) may be
            added around the last container
            -> add a nth 0 direction before the last direction if it is the case
           *)
          (* | Some Delete_instructions ->
             pathl_of_expl_path (List.rev (Dir_nth n :: Dir_nth 0 :: dl)) *)
          | _ -> pathl_of_expl_path (List.rev (Dir_nth n :: dl))
          end
       | _ -> fail t.loc "insert_and_fold: expected a path to a seq element"
     in
     (* replace dx with &dx before folding if we have a reference *)
     fold_decl clog ~as_reference ~fold_at def_pathl t

(* same as insert_and_fold but for types *)
let insert_and_fold_typedef (clog : out_channel)
  ?(insert_before : target = []) ?(insert_after : target = [])
  ?(fold_at : target list = [[]]) (x : typvar) (dx : typ) (t : trm) : trm =
  (* compute the explicit path for later use *)
  let p =
    match insert_before, insert_after with
    | [], _ :: _ ->  insert_after
    | _ :: _, [] ->  insert_before
    | [], [] ->
       fail t.loc "insert_and_fold_typedef: please specify an insertion point"
    | _ ->
       fail t.loc "insert_and_fold_typedef: cannot insert both before and after"
  in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target p t in
  Flags.verbose := b;
  (* insert the typedef *)
  let t = insert_typedef ~insert_before ~insert_after x dx t in
  (*
    fold the typedef
    use explicit path because p will not be resolved as the position of the
    definition
    any path in epl is ok to do so
   *)
  match epl with
  | [] -> fail t.loc "insert_and_fold_typedef: no insertion point"
  | dl :: _ ->
     let dl =
       match List.rev dl with
       | Dir_nth n :: dl' ->
          let n =
            match insert_before, insert_after with
            (* insert after: add 1 to n *)
            | [], _ :: _ -> n + 1
            (* insert before: n is the position of the definition *)
            | _ :: _, [] -> n
            | [], [] ->
               fail t.loc
                 "insert_and_fold_typedef: please specify an insertion point"
            | _ ->
               fail t.loc
                 "insert_and_fold_typedef: cannot insert both before and after"
          in
          List.rev (Dir_nth n :: dl')
       | _ -> fail t.loc "insert_and_fold_typedef: expected a path to a seq"
     in
     let def_pathl = List.map (fun d -> Constr_dir d) dl in
     fold_decl clog ~fold_at def_pathl t

let filteri (f : int -> 'a -> bool) (al : 'a list) : 'a list =
  let aol = List.mapi (fun i a -> if f i a then Some a else None) al in
  List.filter_map (fun ao -> ao) aol

(*
  remove the declaration pointed at by pl
  pl must be resolved as a path to a seq element
  assumption: the declared object is not used in t
 *)
let remove_decl (clog : out_channel) (tr : target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [dl] ->
     (* get the declaration for later use *)
     let (t_decl, _) = resolve_path dl t in
     let log : string =
       let loc : string =
         match t_decl.loc with
         | None -> ""
         | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
       in
       Printf.sprintf
         ("  - expression\n%s\n" ^^
          "    %sis a declaration\n"
         )
         (ast_to_string t_decl) loc
     in
     write_log clog log;
     let log : string =
       let x = decl_name t_decl in
       Printf.sprintf "  - %s is not used in the remainder of the program\n" x
     in
     write_log clog log;
     let dl = List.rev dl in
     let n =
       match List.nth_opt dl 0 with
       | Some (Dir_nth n) -> n
       | _ -> fail t.loc "remove_decl: the path must point at a seq element"
     in
     let t =
       apply_local_transformation
         (fun (t : trm) ->
           match t.desc with
           | Trm_seq tl ->
              let tl = filteri (fun i _ -> i <> n) tl in
              trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add tl
           | _ -> fail t.loc "remove_decl: expected a seq"
         )
         t
         (* remove the last direction to point at the seq *)
         (List.rev (List.tl dl))
     in

     (* remove delete instruction if the declaration is a heap allocation *)
     begin match t_decl.desc with
     (* | Trm_seq _ when t_decl.annot = Some Heap_allocated ->
        let x = decl_name t_decl in
        apply_local_transformation
          (fun (t : trm) ->
            match t.desc with
            | Trm_seq (t_body :: del_instr_l)
                 when t.annot = Some Delete_instructions ->
               let del_instr_l =
                 List.filter
                   (fun (t_del : trm) ->
                     match t_del.desc with
                     | Trm_apps (_, [{desc = Trm_var y; _}]) when y = x -> false
                     | _ -> true
                   )
                   del_instr_l
               in
               begin match del_instr_l with
               | [] -> t_body
               | _ ->
                  trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add
                    ~attributes:t.attributes (t_body :: del_instr_l)
               end
            | _ -> fail t.loc "remove_decl: expected delete instructions"
          )
          t
          (*
            remove the two last directions to point at the seq containing the
            delete instructions
           *)
          (List.rev (List.tl (List.tl dl))) *)
     | _ -> t
     end
  | _ -> fail t.loc "remove_decl: the path must point at exactly 1 subterm"

(* compute a fresh variable (w.r.t. t) based on x *)
let fresh_in (t : trm) (x : var) : var =
  if not (is_used_var_in t x) then x
  else
    begin
      let n = ref 0 in
      while is_used_var_in t (x ^ "_" ^ string_of_int !n) do
        incr n
      done;
      x ^ "_" ^ string_of_int !n
    end

let eliminate_goto_next (t : trm) : trm =
  let rec elim_in_list (tl : trm list) : trm list =
    match tl with
    | t1 :: t2 :: tl ->
       begin match t1.desc, t2.desc with
       | Trm_goto l1, Trm_labelled (l2, _) when l1 = l2 ->
          elim_in_list (t2 :: tl)
       | _ -> t1 :: (elim_in_list (t2 :: tl))
       end
    | _ -> tl
  in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
       trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (elim_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in
  aux t
(* TODO: Change this based on Arthurs'idea *)
let group_decl_init (t : trm) : trm =
  let rec group_in_list (tl : trm list) : trm list =
    match tl with
    | t1 :: t2 :: tl ->
       begin match t1.desc, t2.desc with
       | Trm_seq [{desc = Trm_decl (Def_var ((x, tx), dx)); _}],
         Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                   [{desc = Trm_var y; _}; init])
             when y = x && t1.annot = Some Heap_allocated ->
          let t =
            trm_seq ~annot:(Some Heap_allocated) ~loc:t1.loc
              [
                trm_decl (Def_var ((x, tx), dx));
                trm_set ~annot:(Some Initialisation_instruction) (trm_var x)
                  init
              ]
          in
          group_in_list (t :: tl)
       | _ -> t1 :: (group_in_list (t2 :: tl))
       end
    | _ -> tl
  in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
       trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (group_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in
  aux t

