open Ast
open Target
open Transformations
open Ast_to_c
open Tools 

let create_subsequence_core (clog : out_channel) (label : label) (start_index : int) (stop_path : target) (before_stop : bool) (after_stop : bool) (braces : bool) (t : trm) : trm =
  let rec insert_in_list_at  (el : trm) (i : int) (xs : 'a list) = match xs with
    | [] -> []
    | h :: t as l -> if i = 0 then el :: l else h :: insert_in_list_at el (i-1) t
  in
  let log : string =
    let loc: string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
      ("   - expression\n%s\n" ^^
      " %s is sequence of terms \n"
      )
      (ast_to_string t) loc
      in write_log clog log;
      let epl = resolve_target stop_path t in
      let last_trm = begin match epl with
      | [dl] -> let(l_t,_) = resolve_path dl t in l_t
      | _ -> fail t.loc "create_subsequence_aux: only one exact trm shoudl be matched"
      end
      in match t.desc with
      | Trm_seq tl ->
        let stop_index = get_index last_trm tl in
        let stop_index = match before_stop, after_stop with
        | false,false -> stop_index
        | false, true -> stop_index + 1
        | true, false -> stop_index -1
        | true, true -> fail t.loc "create_subsequence_aux: only one of stop_before or stop_after should be set to true"
        in
        let sub_list = List.rev (foldi(fun i acc x -> if i >= start_index && i <= stop_index then x :: acc else acc) [] tl) in
        let tl = list_remove_set sub_list tl in
        let sub_seq = match braces with
        | true -> trm_seq sub_list
        | false -> trm_seq ~annot:(Some No_braces) sub_list
        in
        let sub_seq =
        if label <> "" then trm_labelled label (sub_seq)
        else
          sub_seq
        in
        let tl = insert_in_list_at sub_seq start_index tl in
        trm_seq ~annot:t.annot tl
      | _ -> fail t.loc "create_subsequence_aux: the sequence which contains the trms was not matched"

(* seq_insert: Insert a list of instructions at the given index as a new sequence
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence) 
      instr: a list of instructions(objects of type trm)
    return: the updated ast 

*)
let seq_insert (clog : out_channel) (path_to_seq : path) (index : int) (instr : trm list) (t : trm): trm =
  let (t,_) = resolve_path path_to_seq t in
  let log : string =
    let loc : string =
    match t.loc with 
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in 
    Printf.sprintf
    (" -expression\n%s\n" ^^
    " %s is sequence of terms \n"
    )
    (ast_to_string t) loc 
    in write_log clog log;
    match t.desc with
    | Trm_seq tl ->
      (* Insert the new sequence with the given instrucitons at index int *)
      let tl = insert_sublist_at instr index tl in 
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "seq_insert: expected the sequence on which the insertion is performed"

(* seq_delete: Remove a list of instructions at the given index as a new sequence(TODO: Ask Arthur if index is needed here)
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence) 
      instr: a list of instructions(objects of type trm)
    return: the updated ast 

*)
let seq_insert (clog : out_channel) (path_to_seq : path) (index : int) (instr : trm list) (t : trm): trm =
  let (t,_) = resolve_path path_to_seq t in
  let log : string =
    let loc : string =
    match t.loc with 
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in 
    Printf.sprintf
    (" -expression\n%s\n" ^^
    " %s is sequence of terms \n"
    )
    (ast_to_string t) loc 
    in write_log clog log;
    match t.desc with
    | Trm_seq tl ->
      (* Remove the trms at those specific indices *)
      (* TODO: Fix function list_remove_set *)
      let tl = list_remove_set instr tl in 
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "seq_insert: expected the sequence on which the insertion is performed"

