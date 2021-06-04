open Ast
open Ast_to_c
open Target
open Tools
open Output

let fold ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.fold as_reference fold_at i t p) tg

let insert ?(const : bool = false) ?(as_reference : bool = false)  (x : var) (dx : trm) (tg : target) : unit =
  Target.apply_on_target_between 
    (fun (p,i) t -> Declaration_core.insert const as_reference x dx i t p) tg

(* same as insert_definition but for a constant *)
let insert_const (x : var) (dx : trm) (tg : target) : unit =
  insert ~const:true x dx tg

let insert_typedef (x : typvar) (dx : typ) (tg : target) : unit =
  Target.apply_on_target_between 
    (fun (p,i) t -> Declaration_core.insert_typedef x dx i t p) tg

(*
  combine insert_definition and fold_decl
  assumption: if x is not a reference, no effects for dx and it has the same
  value through all its occurences
 *)
(* let insert_and_fold (clog : out_channel) ?(insert_before : target = [])
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
     fold_decl clog ~fold_at def_pathl t *)
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
       apply_on_path
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
        apply_on_path
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
       | Trm_seq [{desc = Trm_let (Var_mutable,(x, tx), dx); _}],
         Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                   [{desc = Trm_var _; _}; _])
             (* when y = x && t1.annot = Some Heap_allocated *) ->
          let t =
            trm_let ~loc:t1.loc Var_mutable (x, tx) dx
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

