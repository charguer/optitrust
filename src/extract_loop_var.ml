open Ast
open Paths
open Transformations
open Translate_ast

(*
  extract a variable from a loop:
  - before:
    optional_label:
    for i = 0 to N / for i = N to 0
      var x
      body(x)
  - after:
    result_label:
    {
      var x[N+1]
      optional_label:
      for i = 0 to N / for i = N to 0
        body(x[i])
    }
  assumptions:
    + no initialisation for x in its declaration
    + x is heap allocated and deleted last in the loop
    + the path points to the loop
 *)

let rec split_list_at (n : int) (al : 'a list) : 'a list * ('a list) =
  if n <= 0 then ([], al)
  else
    match al with
    | [] -> failwith "split_list_at: not enough elements"
    | a :: al ->
       let (al, al') = split_list_at (n - 1) al in
       (a :: al, al')

(*
  extract nb_vars var declarations from the loop
  they are expected to be heap allocated and at the beginning of the loop body
 *)
let extract_vars_from_loop (clog : out_channel) (nb_vars : int)
  (loop_labels : string list) (t : trm) : trm =
  match t.desc with
  | Trm_for (init, cond, step, body) ->
     let log : string =
       let loc : string =
         match body.loc with
         | None -> ""
         | Some (_,line1,line2)  -> Printf.sprintf  "at  lines %d  %d " line1 line2
       in
       Printf.sprintf
         ("  - for (%s; %s; %s) is of the form\n" ^^
          "      for ([int] i = 0; i < N; i++) or " ^^
            "for ([int] i = N; i > 0; i--)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sstarts with %d variable declaration(s) where each declared " ^^
            "variable:\n" ^^
          "      + is not initialised in its declaration\n" ^^
          "      + is not a const variable\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc nb_vars
     in
     write_log clog log;
     begin match body.desc with
     | Trm_seq tl when body.annot = Some Delete_instructions ->
        (* the variables are expected to be deleted last *)
        let (var_del_l, tl) =
          let (var_del_l, tl) = split_list_at nb_vars (List.rev tl) in
          (List.rev var_del_l, List.rev tl)
        in
        begin match tl with
        | {desc = Trm_seq tl'; _} :: tl'' ->
           let (var_decl_l, tl') = split_list_at nb_vars tl' in
           let n = for_loop_nb_iter t in
           let change_decl (t_decl : trm) : trm =
              let x = decl_name t_decl in
              let tx =
                let tx = var_decl_type t_decl in
                match tx.ty_desc with
                | Typ_ptr tx' -> tx'
                | _ ->
                   fail t_decl.loc
                     "extract_vars_from_loop: expected heap allocated variable"
              in
              let tx' = typ_array tx (Trm n) in
              trm_seq ~annot:(Some Heap_allocated)
                [
                  trm_decl (Def_var ((x, typ_ptr tx'),
                                     trm_prim (Prim_new tx')))
                ]
           in
           let var_decl_l = List.map change_decl var_decl_l in
           let i = for_loop_index t in
           let change_body (tl' : trm list) (t_decl : trm) : trm list =
              let x = decl_name t_decl in
              let x_i =
                trm_apps (trm_binop Binop_array_access) [trm_var x; trm_var i]
              in
              let rec x_to_xi (t' : trm) : trm =
                (*
                  since x is heap allocated, we either have x alone or get x
                  in both cases: replace x with array_access x i
                  for get x: also replace the annotation with Access
                 *)
                match t'.desc with
                | Trm_var y when y = x -> x_i
                | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _},
                            [{desc = Trm_var y; _}])
                     when t'.annot = Some Heap_allocated && y = x ->
                   trm_apps ~annot:(Some Access) (trm_unop Unop_get) [x_i]
                | _ -> trm_map x_to_xi t'
              in
              List.map x_to_xi tl'
           in
           let tl' = List.fold_left change_body tl' var_decl_l in
           let body' =
             match tl'' with
             | [] -> trm_seq tl'
             | _ ->
                trm_seq ~annot:(Some Delete_instructions)
                  (trm_seq tl' :: tl'')
           in
           (* label the loop if required *)
           let t_loop =
             let t_loop = trm_for init cond step body' in
             match loop_labels with
             | [] -> t_loop
             | _ ->
                List.fold_left (fun t l -> trm_labelled l t) t_loop
                  loop_labels
           in
           trm_seq ~annot:(Some Delete_instructions)
             ((trm_seq (var_decl_l ++ [t_loop])) :: var_del_l)
        | _ -> fail t.loc "extract_vars_from_loop: bad body"
        end
     | _ -> fail t.loc "extract_vars_from_loop: bad delete instructions"
     end
  | _ -> fail t.loc "extract_vars_from_loop: not a for loop"

let nb_decl_vars (t : trm) : int =
  let rec aux (tl : trm list) : int =
    match tl with
    | [] -> 0
    | t :: tl ->
       begin match t.desc with
       | Trm_decl (Def_var _) -> (aux tl) + 1
       | Trm_seq _ when t.annot = Some Heap_allocated -> (aux tl) + 1
       | _ -> 0
       end
  in
  match t.desc with
  | Trm_for (_, _, _, body) ->
     begin match body.desc with
     | Trm_seq ({desc = Trm_seq tl; _} :: _)
          when body.annot = Some Delete_instructions ->
        aux tl
     | Trm_seq tl -> aux tl
     | _ -> fail body.loc "nb_decl_vars: bad loop body"
     end
  | _ -> fail t.loc "nb_decl_vars: expected a for loop"

let rec extract_loop_vars_aux (clog : out_channel) ?(only_one : bool = false)
  ?(loop_labels : string list = []) (result_label : string) (t : trm) : trm =
  match t.desc with
  (*
    if the loop declares its own index, a seq with a delete instruction occurs
    in this case, put the delete instruction back on the loop
   *)
  | Trm_seq [t_loop; t_del_index] when t.annot = Some Delete_instructions ->
     let nb_vars = if only_one then 1 else nb_decl_vars t_loop in
     if nb_vars = 0 then
       trm_labelled result_label
         (List.fold_left (fun t l -> trm_labelled l t) t_loop loop_labels)
     else
       let t' = extract_vars_from_loop clog nb_vars loop_labels t_loop in
       begin match t'.desc with
       | Trm_seq ({desc = Trm_seq tl; _} :: var_del_l)
            when t'.annot = Some Delete_instructions ->
          let (var_decl_l, t_loop') =
            let (var_decl_l, tl) = split_list_at nb_vars tl in
            match tl with
            | [t_loop] -> (var_decl_l, t_loop)
            | _ -> fail t'.loc "extract_loop_vars_aux: expected a loop"
          in
          (* the loop might be labelled *)
          let rec add_del (t_loop : trm) : trm =
            match t_loop.desc with
            | Trm_labelled (l, t_loop) -> trm_labelled l (add_del t_loop)
            | _ ->
               trm_seq ~annot:(Some Delete_instructions) [t_loop; t_del_index]
          in
          let t_loop'' = add_del t_loop' in
          trm_labelled result_label
            (trm_seq ~annot:(Some Delete_instructions)
               ((trm_seq (var_decl_l ++ [t_loop''])) :: var_del_l))
     | _ -> fail t.loc "extract_loop_vars_aux: bad loop var extraction"
     end
  (* otherwise, only extract *)
  | Trm_for _ ->
     let nb_vars = if only_one then 1 else nb_decl_vars t in
     if nb_vars = 0 then
       trm_labelled result_label
         (List.fold_left (fun t l -> trm_labelled l t) t loop_labels)
     else
       trm_labelled result_label
         (extract_vars_from_loop clog nb_vars loop_labels t)
  (* the loop might be labelled *)
  | Trm_labelled (l, t) ->
     extract_loop_vars_aux clog ~loop_labels:(l :: loop_labels) result_label t
  | _ -> fail t.loc "extract_loop_vars_aux: not a for loop"

let extract_loop_vars_aux (clog : out_channel) ?(only_one : bool = false)
  ?(loop_labels : string list = []) (result_label : string) (t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,line1,line2)  -> Printf.sprintf  "at  lines %d  %d " line1 line2
    in
    Printf.sprintf
      ("  - expression\n%s\n" ^^
       "    %sis a (labelled) loop\n"
      )
      (ast_to_string t) loc
  in
  write_log clog log;
  extract_loop_vars_aux clog ~only_one ~loop_labels result_label t

let extract_loop_var (clog : out_channel) (result_label : string)
  (pl : path list) (t : trm) : trm =
  let p = List.flatten pl in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "extract_loop_var: no matching subterm for path %s\n"
       (string_of_path p);
     t
  | [dl] ->
     apply_local_transformation
       (extract_loop_vars_aux clog ~only_one:true result_label) t dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i ->
         apply_local_transformation
           (extract_loop_vars_aux clog ~only_one:true
              (result_label ^ "_" ^ string_of_int i))
       )
       t
       epl

(* extract all possible vars *)
let extract_loop_vars (clog : out_channel) (result_label : string)
  (pl : path list) (t : trm) : trm =
  let p = List.flatten pl in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "extract_loop_vars: no matching subterm for path %s\n"
       (string_of_path p);
     t
  | [dl] ->
     apply_local_transformation (extract_loop_vars_aux clog result_label) t dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i ->
         apply_local_transformation
           (extract_loop_vars_aux clog (result_label ^ "_" ^ string_of_int i))
       )
       t
       epl
