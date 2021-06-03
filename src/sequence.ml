open Ast
open Target
open Ast_to_c
open Tools
open Output


(* Example: [Sequence_core.insert [t1;t2] [cAfter; cFor "i"; cIntr "x ="] *)

let seq_insert (tg : target) (ts : trm list) : unit =
  Target.apply_on_target_between (fun (p,i) t ->
    Sequence_core.insert i ts p t) tg

let seq_delete (ts : trm list) : Transfo.t =
  Target.apply_on_target(Sequence_core.delete ts)


let seq_sub (i : int) (ts : trm list) : Transfo.t =
  Target.apply_on_target( Sequence_core.sub i ts )

let seq_inline (i : int) : Transfo.t =
  Target.apply_on_target(Sequence_core.inline i)

let seq_wrap (visible : bool) : Transfo.t =
  Target.apply_on_target (Sequence_core.wrap visible)

let seq_unwrap : Transfo.t =
  Target.apply_on_target(Sequence_core.unwrap)








(*
  split the sequence t at its n-th instruction
  property: the result is of the form
    result_label:
    {var (split_name x0) decl
    …
    var (split_name xn) decl
    block1_label: {block 1; split_name x0 = x0; …; split_name xn = xn}
    block2 label:
    {var x0 decl = split_name x0; …; var xn decl = split_name xn; block 2}}
  where x0, …, xn are the vars declared in block1 that are used in block 2
  we call them split variables below
  split_name x0, …, split_name xn are heap allocated

  WARNING: the sequence is probably inside another one:
    {seq to split; delete instructions}
  the delete instructions correspond to variables declared in seq (either in
  block 1 or in block 2)
  they are placed inside the seq around block 1/2, selecting the appropriate
  instructions for each block
 *)
(* WARNING: This functioin is recursive but since delete instructions are disabled temporarly
    there is not inner call hence rec flag was removed.
 *)
let split_seq_at (n : int) (result_label : string) (block1_label : string)
  (block2_label : string) (split_name : string -> string) (t : trm) : trm =
  match t.desc with
  (* first case: {seq to split; delete instructions} *)
  (* | Trm_seq tl when t.annot = Some Delete_instructions ->
     begin match tl with
     | t' :: del_instr ->
        (* first split the seq *)
        let t'' =
          split_seq_at n result_label block1_label block2_label split_name t'
        in
        (* then put the appropriate delete instructions around each block *)
        let add_delete_instr (block : trm) : trm =
          (* don't forget to move the label *)
          match block.desc with
          | Trm_labelled (l, block) ->
             trm_labelled l
               (trm_seq (* ~annot:(Some Delete_instructions) *)
                  (block ::
                     List.filter
                       (fun t_del -> is_used_var_in block (deleted_var t_del))
                       del_instr
                  )
               )
          | _ -> fail block.loc "split_seq_at: missing label in recursive call"
        in
        begin match t''.desc with
        (* first case: there are some split variables *)
        | Trm_labelled
          (l,
           {desc = Trm_seq ({desc = Trm_seq tl'; _} :: del_var_copies);
            (* annot = Some Delete_instructions; *) _}) ->
           begin match List.rev tl' with
           | block2 :: block1 :: rintro ->
              trm_labelled l
                (trm_seq (* ~annot:(Some Delete_instructions) *)
                   (trm_seq
                      ((List.rev rintro) ++
                         [add_delete_instr block1; add_delete_instr block2]
                      ) ::
                      del_var_copies
                   )
                )
           | _ -> fail t.loc "split_seq_at: bad recursive call"
           end
        (* other case: no split variable *)
        | Trm_labelled (l, {desc = Trm_seq [block1; block2]; _}) ->
           trm_labelled l
             (trm_seq [add_delete_instr block1; add_delete_instr block2])
        | _ -> fail t.loc "split_seq_at: bad recursive call"
        end
     | _ -> fail t.loc "split_seq_at: bad delete list"
     end *)
  (* second case: seq to split *)
  | Trm_seq tl ->
     let (block1, block2) = split_list_at n tl in
     (* get the list of declarations of split variables *)
     let dl =
       List.filter
         (fun t -> is_used_var_in (trm_seq block2) (decl_name t))
         (var_declarations block1)
     in
     begin match dl with
     (* if there is no split variable, the blocks are independent *)
     | [] ->
        trm_labelled result_label
          (trm_seq
             [trm_labelled block1_label (trm_seq block1);
              trm_labelled block2_label (trm_seq block2)]
          )
     (* otherwise we need to deal with split variables *)
     | _ ->
        (* first compute the list of declarations of split variables copies *)
        let intro =
          List.map
            (fun t ->
              let y = decl_name t in
              let ty = var_decl_type t in
              (* use heap allocation to be allowed to modify the variables *)
              let decl =
                if is_heap_alloc t then
                  begin match ty.ty_desc with
                  | Typ_ptr ty' ->
                    trm_let Var_mutable (split_name y, ty)
                                        (trm_prim (Prim_new ty'))

                  | _ -> fail t.loc "split_seq_at: bad type for heap allocation"
                  end
                else
                  trm_let Var_immutable (split_name y, typ_ptr ty)
                                     (trm_prim (Prim_new ty))

              in decl
              (* trm_seq ~annot:(Some Heap_allocated) [decl] *)
            )
            dl
        in
        (* then initialise the copies in block1 *)
        let block1 =
          block1 ++
            List.map
              (fun t ->
                let y = decl_name t in
                let init =
                  if is_heap_alloc t then
                    trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get)
                      [trm_var y]
                  else
                    trm_var y
                in
                trm_set (trm_var (split_name y)) init
              )
              dl
        in
        (* then declare the variables in block2 *)
        let block2 =
          (List.map
             (fun t ->
               let y = decl_name t in
               let ty = var_decl_type t in
               if is_heap_alloc t then
                 begin match ty.ty_desc with
                 | Typ_ptr ty' ->
                    trm_let Var_mutable (y,ty') (trm_var (split_name y))

                    (* trm_seq ~annot:(Some Heap_allocated)
                      [
                        trm_decl (Def_var ((y, ty), trm_prim (Prim_new ty')));
                        trm_set ~annot:(Some Initialisation_instruction)
                          (trm_var y)
                          (trm_apps ~annot:(Some Heap_allocated)
                             (trm_unop Unop_get) [trm_var (split_name y)])
                      ] *)
                 | _ -> fail t.loc "split_seq_at: bad type for heap allocation"
                 end
               else
                 trm_let Var_mutable (y, ty) (trm_var (split_name y))
                 (* trm_decl
                   (Def_var ((y, ty),
                             trm_apps ~annot:(Some Heap_allocated)
                               (trm_unop Unop_get) [trm_var (split_name y)])) *)
             )
             dl
          ) ++
            block2
        in
        (* finally delete the copies in reverse order *)
        (* let concl =
          List.rev_map
            (fun t ->
              let y = decl_name t in
              trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                (trm_unop (Unop_delete false)) [trm_var y]
            )
            dl
        in *)
        trm_labelled result_label
          (trm_seq (* ~annot:(Some Delete_instructions) *)
             ((trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add
                 (intro ++
                    [trm_labelled block1_label (trm_seq block1);
                     trm_labelled block2_label (trm_seq block2)]
                 )
              ) :: []
              (* ::
              concl *)
             )
          )
     end
  | _ -> fail t.loc "split_seq_at: not a seq"

(*
  split the sequence around the instruction pointed by pl in t
  split_name is used to name introduced vars (see doc of split_seq_at)
  labels: used to define labels for split_seq_at
    - labels = [] -> (result, result_block1, result_block2)
    - labels = [s] -> (s, s_block1, s_block2)
    - labels = [l1, l2, l3] -> (l1, l2, l3)
 *)
let split_sequence (clog : out_channel) (result_label : string)
  (block1_label : string) (block2_label : string)
  (split_name : string -> string) (tr : target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  let app_transfo (result_label : string) (block1_label : string)
    (block2_label : string) (t' : trm) (dl : path) : trm =
    let log : string =
      let (t, _) = resolve_path dl t' in
      let loc : string =
        match t.loc with
        | None -> ""
        | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in
      Printf.sprintf
        ("  - %s, %s and %s are fresh labels\n" ^^
         "  - expression\n%s\n" ^^
         "    %sis located inside a sequence\n"
        )
        result_label block1_label block2_label (ast_to_string t) loc
    in
    write_log clog log;
    match List.rev dl with
    (* the path must point to an instruction in a seq *)
    | Dir_nth n :: dl' ->
       (*
         check if there is a list of delete instructions to take into
         account
        *)
       begin match dl' with
       (*
         to find a list of delete instructions, there must be a seq with
         the appropriate annotation
        *)
       | Dir_nth _ :: dl'' ->
          let dl = List.rev dl'' in
          let (t'', _) = resolve_path dl t' in
          begin match t''.annot with
          (* if there are delete instructions, pass them to split_seq_at *)
          (* | Some Delete_instructions ->
             apply_on_path
               (split_seq_at n result_label block1_label block2_label
                  split_name)
               t'
               dl *)
          (* otherwise, just pass the inner seq *)
          | _ ->
             let dl = List.rev dl' in
             apply_on_path
               (split_seq_at n result_label block1_label block2_label
                  split_name)
               t'
               dl
          end
       | _ ->
          let dl = List.rev dl' in
          apply_on_path
            (split_seq_at n result_label block1_label block2_label
               split_name)
            t'
            dl
       end
    | _ ->
       fail t.loc ("split_sequence: " ^ (path_to_string dl) ^
                     " does not point to a sequence")
  in
  match epl with
  | [] ->
     print_info t.loc "split_sequence: no matching subterm\n";
     t
  | [dl] -> app_transfo result_label block1_label block2_label t dl
  | _ ->
     (*
       folding works since no path in epl is the prefix of a subsequent path
      *)
     foldi
       (fun i t' dl ->
         let (result_label, block1_label, block2_label) =
           let index = string_of_int i in
           (result_label ^ "_" ^ index, block1_label ^ "_" ^ index,
            block2_label ^ "_" ^ index)
         in
         app_transfo result_label block1_label block2_label t' dl
       )
       t
       epl


