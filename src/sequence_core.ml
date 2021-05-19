open Ast
open Target
open Transformations
open Ast_to_c
open Tools 

let create_subsequence_core (clog : out_channel) (label : label) (start_index : int) (stop_path : target) (before_stop : bool) (after_stop : bool) (braces : bool) (t : trm) : trm =
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
      index: an integer in range 0 .. (current number of instructions inside the sequence) 
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
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "seq_insert: expected the sequence on which the insertion is performed"

(* seq_delete: Remove a list of instructions at the given index as a new sequence(TODO: Ask Arthur if index is needed here)
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instructions inside the sequence) 
      instr: a list of instructions(objects of type trm)
    return: the updated ast 

*)
let seq_delete (clog : out_channel) (path_to_seq : path) (instr : trm list) (t : trm): trm =
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
      (* Remove trms*)
      let tl = list_remove_set instr tl in 
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "seq_delete: expected the sequence on which the trms are deleted"

(* seq_sub: Group the targeted instructions into one nested seq term.
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence) 
      instr: a list of instructions(objects of type trm)
    return: the updated ast 
*)
let seq_sub (clog : out_channel) (path_to_seq : path) (index : int) (instr : trm list) (t : trm): trm =
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
      (* First we remove this trms from the sequence *)
      let tl = list_remove_set instr tl in 
      (* Create the inner sequence*)
      let sub_seq = trm_seq instr in 
      (* Insert at the given index the new trm *)
      let tl = insert_in_list_at sub_seq index tl in 
      (* Apply changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "seq_sub: expected the sequence on which the grouping is performed"

(* seq_inline: Inline the inner sequence into the outer one.
    params:
      path_to_seq: explicit path towards the sequence
      index: an integer in range 0 .. (current number of instrucitons inside the sequence) 
    return: the updated ast 
*)
let seq_inline (clog : out_channel) (path_to_seq : path) (index : int) (t : trm): trm =
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
      (* Get the trms from the inner sequence *)
      let inner_seq = List.nth tl index in 
      let inner_seq_trms = begin match inner_seq.desc with 
      | Trm_seq tl -> tl 
      | _ -> fail t.loc "seq_inline: inner sequence was not found, make sure the index is correct"
      end
      in 
      (* Insert at the given index the trms from the inner sequence *)
      let tl = insert_sublist_in_list inner_seq_trms index tl in 
      (* Apply the changes *)
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "seq_inline: expected the sequence on which the ilining is performed"

(* seq_wrap: Turn the given instruction into a sequence containing the given instruction
    params:
      path_to_instr: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not 
    return: the updated ast 
*)
let seq_wrap (clog : out_channel) (path_to_instr : path) (visible : bool) (t : trm): trm =
  let (t,_) = resolve_path path_to_instr t in
  let log : string =
    let loc : string =
    match t.loc with 
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in 
    Printf.sprintf
    (" -expression\n%s\n" ^^
    " %s is an instruction \n"
    )
    (ast_to_string t) loc 
    in write_log clog log;
    trm_seq ~annot:(if not visible then Some No_braces else None) [t]


(* seq_unwrap: The inverse of seq_wrap , remove the sequence and replace it directly with the trms it contains
    params:
      path_to_seq: explicit path towards the sequence
      visible: a boolean to decide if the wraped sequence should be visible or not 
    return: the updated ast 
*)
let seq_unwrap (clog : out_channel) (path_to_seq : path) (t : trm): trm =
  let (t,_) = resolve_path path_to_seq t in
  let log : string =
    let loc : string =
    match t.loc with 
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in 
    Printf.sprintf
    (" -expression\n%s\n" ^^
    " %s is a sequence \n"
    )
    (ast_to_string t) loc 
    in write_log clog log;
    match t.desc with 
    | Trm_seq [el] -> el
    | _ -> fail t.loc "seq_unwrap: expected the sequence wanted to remove, the sequence shoudl contain only one trm"


(* TODO: Implement later seq_distrib_ref after references have been implemented *)
