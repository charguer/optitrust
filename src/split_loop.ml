open Ast
open Paths
open Transformations
open Translate_ast

(*
  split the for loop pointed by pl in t
  assumption: the loop is of the form
  optional_label:
  for i = 0 to N / for i = N to 0
    {block1}
    {block2}
  where block1 and block2 are independent
  result:
    result_label:{
      loop1_label:
      for i = 0 to N / for i = N to 0
        block1
      loop2_label:
      for i = 0 to N / for i = N to 0
        block2
    }
 *)

let rec split_loop_nodep_aux (clog : out_channel) (result_label : string)
  (loop1_label : string) (loop2_label : string) (t : trm) : trm =
  match t.desc with
  (*
    if the loop declares its own index, a seq with a delete instruction occurs
    in this case, duplicate the delete instruction for the two resulting loops
   *)
  | Trm_seq [t_loop; t_del] when t.annot = Some Delete_instructions ->
     let t' =
       split_loop_nodep_aux clog result_label loop1_label loop2_label t_loop
     in
     begin match t'.desc with
     | Trm_labelled (l, {desc = Trm_seq [t_loop1; t_loop2]; _}) ->
        let add_del_instr (t_loop : trm) : trm =
          match t_loop.desc with
          | Trm_labelled (l', t_loop) ->
             trm_labelled l'
               (trm_seq ~annot:(Some Delete_instructions) [t_loop; t_del])
          | _ -> fail t_loop.loc "split_loop_nodep_aux: expected labelled loop"
        in
        trm_labelled l (trm_seq [add_del_instr t_loop1; add_del_instr t_loop2])
     | _ -> fail t.loc "split_loop_nodep_aux: bad loop splitting"
     end
  (* otherwise, just split *)
  | Trm_for (init, cond, step, body) ->
     let log : string =
       let loc : string =
         match body.loc with
         | None -> ""
         | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
       in
       Printf.sprintf
         ("  - for (%s; %s; %s) is of the form\n" ^^
          "      for ([int] i = 0; i < N; i++) or " ^^
            "for ([int] i = N; i > 0; i--)\n" ^^
          "  - expression\n%s\n" ^^
          "    %sis of the form\n" ^^
          "      {\n" ^^
          "        {block1}\n" ^^
          "        {block2}\n" ^^
          "      }\n"
         )
         (ast_to_string init) (ast_to_string cond) (ast_to_string step)
         (ast_to_string body) loc
     in
     write_log clog log;
     begin match body.desc with
     | Trm_seq [t_block1; t_block2] ->
        let log : string =
          Printf.sprintf
            ("  - blocks\n%s\n" ^^
             "    and\n%s\n" ^^
             "    are independent\n"
            )
            (ast_to_string t_block1)
            (ast_to_string t_block2)
        in
        write_log clog log;
        trm_labelled result_label
          (trm_seq
             [
               trm_labelled loop1_label (trm_for init cond step t_block1);
               trm_labelled loop2_label (trm_for init cond step t_block2)
             ]
          )
     | _ -> fail t.loc "split_loop_nodep_aux: bad loop body"
     end
  (* the loop might be labelled: delete the label *)
  | Trm_labelled (_, t_loop) ->
     split_loop_nodep_aux clog result_label loop1_label loop2_label t_loop
  | _ -> fail t.loc "split_loop_nodep_aux: not a for loop"

let split_loop_nodep_aux (clog : out_channel) (result_label : string)
  (loop1_label : string) (loop2_label : string) (t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
    Printf.sprintf
      ("  - %s, %s and %s are fresh labels\n" ^^
       "  - expression\n%s\n" ^^
       "    %sis a (labelled) loop\n"
      )
      result_label loop1_label loop2_label (ast_to_string t) loc
  in
  write_log clog log;
  split_loop_nodep_aux clog result_label loop1_label loop2_label t

let split_loop_nodep (clog : out_channel) (result_label : string)
  (loop1_label : string) (loop2_label : string) (pl : path list)
  (t : trm) : trm =
  let p = List.flatten pl in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_path p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "split_loop_nodep: no matching subterm\n";
     t
  | [dl] ->
     apply_local_transformation
       (split_loop_nodep_aux clog result_label loop1_label loop2_label)
       t
       dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i ->
         let (result_label, loop1_label, loop2_label) =
           let index = string_of_int i in
           (result_label ^ index, loop1_label ^ index, loop2_label ^ index)
         in
         apply_local_transformation
           (split_loop_nodep_aux clog result_label loop1_label loop2_label)
       )
       t
       epl
