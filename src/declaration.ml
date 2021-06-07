open Ast
open Target

let fold ?(as_reference : bool = false) ?(fold_at : target list = [[]]) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Declaration_core.fold as_reference fold_at i t p) tg

let insert ?(const : bool = false) ?(as_reference : bool = false)  (x : var) (dx : trm) (tg : target) : unit =
  Target.apply_on_target_between 
    (fun (p,i) t -> Declaration_core.insert const as_reference x dx i t p) tg

(* same as insert but for a constant *)
let insert_const (x : var) (dx : trm) (tg : target) : unit =
  insert ~const:true x dx tg

let insert_typedef (x : typvar) (dx : typ) (tg : target) : unit =
  Target.apply_on_target_between 
    (fun (p,i) t -> Declaration_core.insert_typedef x dx i t p) tg

let remove : Transfo.t =
  Target.apply_on_target(Declaration_core.remove)

(* TODO: Implement insert_and_fold *)
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

