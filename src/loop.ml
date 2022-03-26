open Ast
open Target

include Loop_basic

type rename = Variable.Rename.t
(* [hoist x_step tg] - expects target to point inside the declaration of the variable
    [x_step] - denotes the variable going to be hoisted outside the loop.
    This transformation is similar to the basic one except that it supports also
    undetached declarations contrary to the basic one. This is done by first checking if
    the declaration is detached or not. If it is not detached then we call another
    transformation which does that for us. Otherwise just apply the basic hoist transformation.
*)
let hoist ?(name : var = "${var}_step") ?(array_size : trm option = None) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
      let detach_first =
      match tg_trm.desc with
        | Trm_let (_, (_, _), init) ->
          begin match init.desc with
          | Trm_val(Val_lit (Lit_uninitialized)) -> false
          | Trm_val(Val_prim (Prim_new _))-> false
          | _ -> true
          end
        | _ -> fail tg_trm.loc "hoist: expected a variable declaration"
        in
        match detach_first with
        | true ->
          Variable_basic.init_detach (Target.target_of_path p);
          Loop_basic.hoist ~name ~array_size(Target.target_of_path p);
        | false -> Loop_basic.hoist ~name ~array_size (Target.target_of_path p)
  ) tg

(* [fusion nb tg] expects [tg] to point to a for loop followed by one or more
    for loops with the same range, start step and bound but different body.
    Then it's going to merge bodies of all those loops into a single loop.
    [nb] - denotes the number of loops to consider. *)
let fusion ?(nb : int = 2) (tg : Target.target) : unit =
  let mark = "__TEMP_MARK" in
  Sequence_basic.intro nb ~mark tg;
  Loop_basic.fusion_on_block [Target.cMark mark]

(* [fusion_targets tg] expects the target [tg] to be pointing at a sequence that contains loops
    then it will move all the other instructions other than loops outside that sequence.
    After that, it will call fusion in block.

    Assumptions:
      The loops inside the sequence satisfy the same assumption as in fusion_in_block transformation
      All the instructions in-between loops should not depend on the index of the loop.
*)

let fusion_targets (tg : Target.target) : unit =
  let non_loop_indices = ref [] in
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in

    let aux (tl : trm mlist) : unit =
      Mlist.iteri (fun i t1 ->
        match t1.desc with
        | Trm_for _ -> ()
        | _ -> non_loop_indices := i :: !non_loop_indices
      ) tl
     in
    match tg_trm.desc with
    | Trm_seq tl ->
      aux tl
    | Trm_labelled (l, t1) ->
      begin match t1.desc with
      | Trm_seq tl -> aux tl
      | _ -> fail t.loc" fusion_targets: expected a labelled sequence or a direct target to a sequence"
      end
    | _ -> fail tg_trm.loc (Printf.sprintf "fusion_targets: expected a target pointing to the sequence that contains the potential loops to be fused, %s" (Ast_to_text.ast_to_string tg_trm))

  ) tg;
  List.iteri (fun i index -> Instr.move_out ~dest:([Target.tBefore] @ tg) (tg @ [Target.dSeqNth (index-i)])) (List.rev !non_loop_indices);
  Loop_basic.fusion_on_block tg


(* [move_out ~upto  tg] expects the target [tg] pointing to an instruction inside a for loop
    then it will move that instruction outside that loop. In the case of nested loops the user
    can specify before which loop with index [upto] wants the instruction to be moved to.
*)
let move_out ?(upto : string = "") (tg : Target.target) : unit =
  Internal.nobrace_remove_after( fun _ ->
  Target.iter_on_targets (fun t exp ->
    let (p, _) = Internal.get_trm_in_surrounding_loop exp in
    match upto with
    | "" -> Loop_basic.move_out tg
    | _ ->
          let quit_loop = ref false in
          let tmp_p = ref [] in
          tmp_p := List.rev(List.tl (List.rev p));
          while not !quit_loop do
            let tg_trm = Path.resolve_path !tmp_p t in
            match  tg_trm.desc with
            | Trm_for _ ->
              let index = for_loop_index tg_trm in
              if index = upto then
                  begin
                  Loop_basic.move_out tg;
                  quit_loop := true;
                  end
                else
                  Loop_basic.move_out tg;
                  tmp_p := List.rev(List.tl (List.rev !tmp_p))
            | _ ->
              Loop_basic.move_out tg;
              tmp_p := List.rev(List.tl (List.rev !tmp_p))
            done
  ) tg
)

(* [move before after loop_to_move] move one loop before or after another loop in
    a "sequence"(not in the context of Optitrust) of nested loops.
    [before] - a default argument given as empty string, if the user wants to move
      [loop_to_move] before another loop then it should use this default argument with the
      value the quoted loop index
    [after] - similar to [before] but now is the index of the loop after whom
      we want to move [loop_to_move]
*)
let move ?(before : Target.target = []) ?(after : Target.target = []) (loop_to_move : Target.target) : unit =
  Trace.call (fun t ->
   let loop_to_move_path = Target.resolve_target_exactly_one_with_stringreprs_available loop_to_move t in
   let loop_to_move_trm = Path.resolve_path loop_to_move_path t in
   let loop_to_move_nested_indices = Internal.get_loop_nest_indices loop_to_move_trm in
   let loop_to_move_index  = List.nth loop_to_move_nested_indices 0 in
   begin match before, after with
   | [], [] -> fail None  "move: the before target or after target are mandatory please enter only one of them"
   | [], _ ->
    let targeted_loop_path = Target.resolve_target_exactly_one_with_stringreprs_available after t in
    let targeted_loop = Path.resolve_path targeted_loop_path t in
    let targeted_loop_nested_indices = Internal.get_loop_nest_indices targeted_loop in
    let targeted_loop_index = List.nth targeted_loop_nested_indices  0 in
    if List.mem targeted_loop_index loop_to_move_nested_indices
      then begin
           let choped_indices = Tools.list_chop_after targeted_loop_index loop_to_move_nested_indices in
           List.iter (fun _ -> Loop_basic.swap loop_to_move) choped_indices
           end
      else if List.mem loop_to_move_index targeted_loop_nested_indices then
        begin
        let choped_indices = Tools.list_chop_after loop_to_move_index targeted_loop_nested_indices in
        let choped_indices = Tools.list_chop_after targeted_loop_index (List.rev choped_indices) in
        List.iter (fun x -> Loop_basic.swap [Target.cFor x]) choped_indices
        end
      else fail loop_to_move_trm.loc "move: the given targets are not correct"

   | _ , [] ->
    let targeted_loop_path = Target.resolve_target_exactly_one_with_stringreprs_available before t in
    let targeted_loop = Path.resolve_path targeted_loop_path t in
    let targeted_loop_nested_indices = Internal.get_loop_nest_indices targeted_loop in
    let targeted_loop_index = List.nth targeted_loop_nested_indices  0 in
    if List.mem targeted_loop_index loop_to_move_nested_indices
      then begin
           let choped_indices = Tools.list_chop_after targeted_loop_index loop_to_move_nested_indices in
           let choped_indices = Tools.list_chop_after loop_to_move_index (List.rev choped_indices) in
           List.iter (fun _ -> Loop_basic.swap loop_to_move) (List.rev choped_indices)
           end
      else if List.mem loop_to_move_index targeted_loop_nested_indices then
        begin
        let choped_indices = Tools.list_chop_after loop_to_move_index targeted_loop_nested_indices in
        List.iter (fun x -> Loop_basic.swap [Target.cFor x]) (List.rev choped_indices)
        end
      else fail loop_to_move_trm.loc "move: the given targets are not correct"

   | _  -> fail None "move: only one of target before or after should be given"
   end
  )

(*
DETAILS for [unroll]
 Let [tg] target the following loop

    for (int i = a; i < a + N; i++) {

    if N is a variable -> call inline_var on this target
    then
    if N is not a literal -> fail
    then
    call the basic unroll


    [unroll_and_shuffle] which does unroll, then [shuffle]

    [shuffle] is a stand alone transformation (see notes)
STEP 1 (BASIC): ONLY UNROLL

   for i { body(i) }
   --->
   { body(i+0) }
   { body(i+1) }
   { body(i+2) }

  example:
    { int a = (i+0 * 2);
        t[i] = a; }
    { int a = (i+1 * 2);
        t[i] = a; }

  STEP2:  software-pipelining is a combi transformation that decomposes as:

   START:
   {
     { body(i+0) }
     { body(i+1) }
     { body(i+2) }
   }

   FIRST SUBSTEP : perform renaming of local varaibles (see simd.txt)

   SECOND SUBSTEP: make the subgroups
    now with number of instructions in each sublock, e.g. take a list [2;3]
     Sequence.partition [2;3] p    // DONE: test "partition" as a combi transfo
        // -> check that the sum of the sizes in the list correspond to the nb of items in the seq
       -> implemented as
            Sequence.sub 0 2; Sequence.sub 1 3; Sequence.sub 2 ...
          (list fold over the partition sizes)
       -> make the @nobraces on the subsequences produced (this should be a flag of Seq.sub),
          so that we can remove them at the end
       where p points to the item "body(i+k)"

       ( if body(i) is   instr1 instr2 instr3 instr4 instr5
       ( then i make { { instr1 instr2 } { instr3 instr4 instr5 } }

   {
     { { instr1 instr2(i+0) } { instr3 instr4 instr5(i+0) } }
     { { instr1 instr2(i+1) } { instr3 instr4 instr5(i+1) } }
     { { instr1 instr2(i+2) } { instr3 instr4 instr5(i+2) } }
   }
   THIRD SUBSTEP: reorder instructions
   {
     { { instr1 instr2(i+0) }@nobrace
       { instr1 instr2(i+1) }
       { instr1 instr2(i+2) } }@?
     { { instr3 instr4 instr5(i+0) }
       { instr3 instr4 instr5(i+1) }
       { instr3 instr4 instr5(i+2) } }@?
   }

   FOURTH SUBSTEP: remove nobrace sequences


 ===================note
    the actual reorder operation is just (the one already implemented):
    {
     { cmd1(i+0) cmd2 cmd3 }
     { cmd1(i+1) cmd2 cmd3 }
     { cmd1(i+2) cmd2 cmd3 }
   }
   THIRD SUBSTEP: reorder instructions
   {
     cmd1(i+0)
     cmd1(i+1)
     cmd1(i+2)
     cmd2(i+0)
     cmd2(i+1)
     cmd2(i+2)
     cmd3(i+0)
     cmd3(i+1)
     cmd3(i+2)

   }

*)


(* [unroll] expects the target to point to a loop. Then it checks if the loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the
    the instructions out of the loop and the loop will be removed. It works also
    in the case when C = 0 and a is a constant variable. To get the number of steps
    a is first inlined.

    [braces]: a flag on the visiblity of blocks created during the unroll process

    [blocks]: a list of integers describing the partition type of the targeted sequence

    [shuffle]: shuffle blocks

      Assumption: C should be a literal or a constant variable *)
let unroll ?(braces : bool = false) ?(blocks : int list = []) ?(shuffle : bool = false) (tg : Target.target) : unit =
  Target.reparse_after ~reparse:(not braces) (Target.iteri_on_targets (fun i t p ->
    let my_mark = "__unroll_" ^ string_of_int i in
    let tg_loop_trm  = Path.resolve_path p t in
    Marks.add my_mark (Target.target_of_path p);
    (* Function used in the case when the loop bound is a constant variable *)
    let aux (x : var) (t : trm) : int  =
      Variable_basic.unfold ~at:[Target.cMark my_mark] [Target.cVarDef x];
          let var_decl = match Internal.toplevel_decl x with
            | Some d -> d
            | None -> fail t.loc "unroll: could not find the declaration of the loop bound variable"
            in
          let lit_n = match get_init_val var_decl with
          | Some init1 -> init1
          | None -> fail t.loc "unroll: could not get the value of the loop component" in
          match (get_lit_from_trm_lit lit_n) with
          | Lit_int n -> n
          | _ -> fail t.loc "unroll: could not get the number of steps to unroll"
      in
    match tg_loop_trm.desc with
    | Trm_for (_index, start, _direction, stop, _, _) ->
      let nb_instr = begin match stop.desc with
      | Trm_apps (_, [_;bnd]) ->
        begin match bnd.desc with
        | Trm_val (Val_lit (Lit_int n)) -> n
        | Trm_var (_, x) -> aux x t
        | _ -> fail stop.loc "unroll: expected eitehr a constant variable of a literal"
        end
      | Trm_var (_, x) ->
          let start_nb = begin match start.desc with
          | Trm_var (_, y) -> aux y t
          | Trm_val (Val_lit (Lit_int n)) -> n
          | _ -> fail start.loc "unroll: expected a loop of the form for (int i = a; i < N; i where a should be a constant variable"
          end in
          (aux x t) - start_nb
      | Trm_val (Val_lit (Lit_int n)) -> n
      | _ -> fail stop.loc "unroll: expected an addition of two constants or a constant variable"
      end
        in
      Loop_basic.unroll ~braces:true ~my_mark [Target.cMark my_mark];
      let block_list = Tools.range 0 (nb_instr-1) in
      List.iter (fun x ->
        Variable.renames (AddSuffix (string_of_int x)) ([Target.occIndex ~nb:nb_instr x; Target.cMark my_mark;Target.cSeq ()])
      ) block_list;
      List.iter (fun x ->
         Sequence_basic.partition ~braces blocks [Target.cMark my_mark; Target.dSeqNth x]
      ) block_list;
      if shuffle then Sequence_basic.shuffle ~braces [Target.cMark my_mark];
      Marks.remove my_mark [Target.nbAny;Target.cMark my_mark]
    | _ -> fail tg_loop_trm.loc "unroll: expected a loop to unroll"
  )) tg

(* [reorder order]  expects the target [tg] to point to the first loop included in the [order]
    list, then it will find all the nested loops starting from the targeted loop [tg] and
    reorder them based on [oder].
    Assumption:
      All loops have as bodies blocks of code(sequences)

    @correctness: correct if loops are parallelizable
*)
let reorder ?(order : vars = []) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let tg_loop = Path.resolve_path p t in
    let indices = Internal.get_loop_nest_indices tg_loop in
    let nb_order = List.length order in
    if nb_order > List.length indices
      then fail tg_loop.loc "[Loop.reorder]: the number of indices provided in argument [order] exceeds the number of nested loops that appears in the code."
    else if nb_order = 0
      then ()
    else if nb_order = 1 && List.nth order 0 <> List.nth indices 0
      then fail tg_loop.loc "[Loop.reorder]: the single index in the argument [order] should match the name of the targeted loop."
    else
    let _, targeted_loop_index = Tools.unlast order in
    (*  LATER: use more precise targets, to avoid targeting deeply-nested loops that resue the same index *)
    List.iter (fun x -> move (Target.target_of_path p @ [Target.cFor x]) ~before:(Target.target_of_path p @ [Target.cFor targeted_loop_index])) order
  ) tg


(* LATER: tg should be a target on the outer loop, not on its context.
   I think using [tg @ [cForNestedAtDepth i]] would work for targeting the loop at depth i  *)
(* [pic_coloring tile_size color_size ds tg] expects the target [tg] to point to the first loop
      on which tiling(refer to Loop_basic.tile ) is going to be applied. Then on the loop comming right after the target [tg]
      coloring transformation (refer to Loop_basic.color).
      Finally a reorder is going to be applied by using reorder transformation (refer to reorder).
      Assumption:
        The target should be a nested loop
*)
let pic_coloring (tile_size : int) (color_size : int) (ds : string list) (tg : Target.target) : unit =
  let add_prefix (prefix : string) (indices : string list) : string list =
    List.map (fun x -> prefix ^ x) indices
   in
  let bs = add_prefix "b" ds in
  let cs = add_prefix "c" ds in
  let first_cs = List.nth cs 0 in
  let order = cs @ bs @ ds in
  let tile = string_of_int tile_size in
  let color = string_of_int color_size in
  List.iter2 (fun d b -> Loop_basic.tile tile ~index:b (tg @ [Target.cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color (AstParser.expr color) ~index:c (tg @ [Target.cFor b])) bs cs;
  reorder ~order [Target.cFor first_cs]

(* [fission tg] if [split_between] is false then this function just calls Loop_basic.fission otherwise
    it will split the targeted loop into unit instructions
*)
let fission ?(split_between : bool = false) (tg : Target.target) : unit =
  if not split_between
    then Loop_basic.fission tg
    else Internal.nobrace_remove_after(fun _ ->
      Target.apply_on_targets (fun t p ->
        let tg_trm = Path.resolve_path p t in
        match tg_trm.desc with
        | Trm_for (loop_index, start, direction, stop, step, body) ->
          begin match body.desc with
          | Trm_seq tl ->
            let body_lists = List.map (fun t1 -> trm_seq_nomarks [t1] ) (Mlist.to_list tl) in
            Target.apply_on_path (fun t -> trm_seq_no_brace (List.map (fun t1 -> trm_for loop_index start direction stop step t1) body_lists)) t p
          | _ -> fail t.loc "fission_aux: expected the sequence inside the loop body"
          end
        | _ -> fail t.loc "fission_aux: only simple loops are supported") tg)


(* [fold ~index ~start ~step ~nb_instr tg] expects the target [tg] to be pointing to an instruction folloed by [nb_instr] -1 instructions
      which could be expressed into a single for loop with [index], [start], [nb_instr] and [step] as its components.

   @correctness: always correct, as we can map all intermediate predicates
   to numbered predicates on the loop
 *)
let fold  ?(start : int = 0) ?(step : int = 1) ~index:(index : var) (nb_instr : int) (tg : Target.target) : unit =
  let mark = "opti_fold" in
  Sequence_basic.intro ~mark nb_instr tg;
  Loop_basic.fold ~index ~start ~step [Target.cMark mark]


(* [fold_instrs ~index ~start ~step tg] expects the target [tg] pointing to more than one instructions in a sequence
    all this instructions shoudl be consecutive ones. Then it will find the number of targeted instructions and it will call
    the previous transformation [fold]. The difference here is that the number of instructions is computed automatically.
    LATER: Merge this two functions into one
*)
let fold_instrs ~index:(index : var) ?(start : int = 0) ?(step : int = 1) (tg : Target.target) : unit =
  let nb_targets = ref 0 in
  let prev_index = ref (-1) in
  let first_target = [Target.occFirst] @ (Target.filter_constr_occurrence tg) in
  let tg = Target.enable_multi_targets tg in
  Target.iter_on_targets (fun t p ->
      let _, i = Internal.isolate_last_dir_in_seq p in
      if i <> !prev_index + 1 && !prev_index <> -1 then fail t.loc "fold_instrs: all the targeted instructions should be consecutive ones";
      incr nb_targets;
    ) tg;
  if !nb_targets < 1 then fail None "fold_instrs: expected at least 1 instruction";
  fold ~index ~start ~step !nb_targets first_target;
  Variable.fold ~nonconst:true [Target.nbAny;Target.cVarDef "" ~body:[Target.cInt !nb_targets]]

(* [isolate_first_iteration tg] expects the target [tg] to be pointing at a simple loop, then it will
   split that loop into two loops by calling split_range transformation. Finally it will unroll the first 
   loop *)
let isolate_first_iteration (tg : Target.target) : unit = 
  Loop_basic.split_range ~nb:1 tg;
  unroll ([occFirst] @ tg)

(* [grid_enumerate  ~indices tg] similar to Loop_basic.grid_enumerate but this one computes the bounds automatically
     under the assumption that the bound of the targeted loop is given as a product of the bounds for each dimension *)
let grid_enumerate ?(indices : string list = []) : Target.Transfo.t = 
  Target.iter_on_targets (fun t p -> 
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with 
    | Trm_for (index, _, _, stop, _, _) ->
      begin match trm_prod_inv stop with 
      | [] -> fail tg_trm.loc "grid_enumerate: the bound of the targeted loop should be a product of the bounds of each dimension" 
      | bounds -> 
        let indices_and_bounds = 
        if indices = [] then 
          let indices = List.mapi (fun i _ -> index ^ (string_of_int i)) bounds in 
          List.combine indices bounds
        else begin
          if List.length indices <> List.length bounds then fail tg_trm.loc "grid_enumerate: the provided list of indices does 
            not correspond to the shape of the targeted loop bound";
          List.combine indices bounds 
          end
          in
        Loop_basic.grid_enumerate indices_and_bounds (Target.target_of_path p)
      end

    | _ -> fail tg_trm.loc "grid_enumerate: expected a target to a simple loop"
  )

