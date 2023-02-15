open Ast
open Target
include Loop_basic

(* [rename]: instantiation of Rename module *)
type rename = Variable.Rename.t

(* [hoist ~name ~array_size tg]: this transformation is similar to [Loop_basic.hoist] (see loop_basic.ml) except that this
    transformation supports also undetached declarations contrary to the basic one. This is done by first checking if
    the declaration is detached or not. If it is not detached then we call another transformation that does the detachment for us. *)
let hoist1_aux (name : var)
               (mark : mark option)
               (init_to_detach : trm option)
               (p : path) : unit = begin
  let detach_first =
    match init_to_detach with
    | Some init -> 
      not ((is_trm_uninitialized init) || (is_trm_new_uninitialized init))
    | None -> false
  in
  if detach_first then
    Variable_basic.init_detach (target_of_path p);
  Loop_basic.hoist ~name ~mark (target_of_path p);
end

(* LATER/ deprecated *)
let hoist_old ?(name : var = "${var}_step") ?(array_size : trm option = None) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
      let detach_first =
      match tg_trm.desc with
        | Trm_let (_, (_, _), init) ->
          begin match init.desc with
          | Trm_val(Val_lit (Lit_uninitialized)) -> false
          | Trm_val(Val_prim (Prim_new _))-> false
          | _ -> true
          end
        | _ -> fail tg_trm.loc "Loop.hoist: expected a variable declaration"
        in
        match detach_first with
        | true ->
          Variable_basic.init_detach (target_of_path p);
          Loop_basic.hoist_old ~name ~array_size(target_of_path p);
        | false -> Loop_basic.hoist_old ~name ~array_size (target_of_path p)
  ) tg

(* [hoist ~name ~array_size ~inline tg]: this transformation is similar to [Loop_basic.hoist] (see loop_basic.ml) except that this
    transformation supports also undetached declarations as well as hoisting through multiple loops.
    [inline] - inlines the array indexing code
    [nb_loops] - number of loops to hoist through (expecting a perfect nest of simple loops)

    for (int l = 0; l < 5; l++) {
      for (int m = 0; m < 2; m++) {
        int x = ...;
      }
    }
    --> first hoist
    for (int l = 0; l < 5; l++) {
      int* x_step = MALLOC1(2, sizeof(int));
      for (int m = 0; m < 2; m++) {
        int& x = x_step[MINDEX1(2, m)];
        x = ...;
      }
    }
    --> second hoist
    int* x_step_bis = MALLOC2(5, 2, sizeof(int));
    for (int l = 0; l < 5; l++) {
      int*& x_step = x_step_bis[MINDEX2(5, 2, l, 0)];
      for (int m = 0; m < 2; m++) {
        int& x = x_step[MINDEX1(2, m)];
        x = ...;
      }
    }
    --> final
    int* x_step_bis = MALLOC2(5, 2, sizeof(int));
    for (int l = 0; l < 5; l++) {
      for (int m = 0; m < 2; m++) {
        int& x = x_step_bis[MINDEX2(5, 2, l, m)];
        x = ...;
      }
    }
 *)
let hoist ?(tmp_names : string = "${var}_step${i}")
          ?(name : string = "")
          ?(inline : bool = true)
          ?(nb_loops : int = 1)
          (tg : target) : unit =
  let tmp_marks = ref [] in
  let alloc_mark = Mark.next () in
  let rec mark_and_hoist name i (init_to_detach : trm option) (p : path) =
    let next_name = Tools.string_subst "${i}" (string_of_int i) name in
    let (instr_index, seq_path) = Path.index_in_seq p in
    let seq_target = target_of_path seq_path in
    let seq_mark = Mark.next () in
    Marks.add seq_mark seq_target;
    tmp_marks := (seq_mark, instr_index) :: !tmp_marks;

    let more_hoists = i + 1 <= nb_loops in
    let maybe_mark = if more_hoists then None else Some alloc_mark in
    hoist1_aux next_name maybe_mark init_to_detach p;

    let loop_path = snd (Path.index_in_surrounding_loop p) in
    let next_target = (target_of_path loop_path) @ [cVarDef next_name] in
    if more_hoists then
      iter_on_targets (fun t p -> mark_and_hoist name (i + 1) None p) next_target;
  in
  let simpl_index_after_inline (tg : target) : unit =
    Target.iter (fun _t p_nested ->
      let p = p_nested |> Path.parent in
      Matrix_basic.simpl_access_of_access (target_of_path p);
      Matrix_basic.simpl_index_add ((target_of_path p) @ [dArg 1]);
      Arith.(simpl_rec gather_rec ((target_of_path p) @ [dArg 1]));
    ) tg
  in
  let make_pretty_and_unmark (alloc_name: string) : unit =
    List.iteri (fun i (seq_mark, instr_index) ->
      if inline then
        iter_on_targets (fun t loop_p ->
          let instr_target = (target_of_path loop_p) @ [dSeqNth instr_index] in
          let is_partial_mindex = (i + 1) < nb_loops in
          if is_partial_mindex then begin
            Variable_basic.to_const instr_target;
            Marks.with_fresh_mark (fun mark ->
              Variable_basic.inline ~mark instr_target;
              simpl_index_after_inline [cMark mark]
            )
          end else
            Variable_basic.inline instr_target;
        ) [cMark seq_mark];
      Marks.remove seq_mark [cMark seq_mark];
    ) !tmp_marks;
    tmp_marks := [];
    if alloc_name <> "" then
      Variable_basic.rename ~into:alloc_name [cMark alloc_mark];
    Marks.remove alloc_mark [cMark alloc_mark];
  in
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (_, (x, _), init) ->
      if 1 <= nb_loops then begin
        let subs_name = Tools.string_subst "${var}" x tmp_names in
        let alloc_name =
          if inline && (name = "")
          then x
          else Tools.string_subst "${var}" x name
        in
        mark_and_hoist subs_name 1 (Some init) p;
        make_pretty_and_unmark alloc_name;
      end
    | _ -> fail tg_trm.loc "Loop.hoist: expected a variable declaration"
  ) tg

(* [fusion nb tg]: expects the target [tg] to point at a for loop followed by one or more
    for loops with the same range, start, step and bound but different bodies.
    Then it's going to merge the bodies of all those loops into a single loop.
    [nb] - denotes the number of loops to consider. *)
let fusion ?(nb : int = 2) (tg : target) : unit =
  let mark = "__TEMP_MARK" in
  Sequence_basic.intro nb ~mark tg;
  Loop_basic.fusion_on_block [cMark mark]

(* [fusion_targets tg]: similar to [fusion] except that this transformation assumes that [tg] points to multiple
    not neccessarily consecutive for loops. This transformation regroups all this loops into a single sequence
    and then it calls [Loops_basic.fusion_on_block].

    Assumptions:
      The loops inside the sequence satisfy the same assumptions as in [Loop_basic.fusion_in_block] transformation
      All the instructions in-between loops should not depend on the index of the loop. *)

let fusion_targets ?(keep_label : bool = true) : Transfo.t =
  iteri_on_targets (fun i t p ->
    Marks.add "mark_seq" (target_of_path p);
    let mark = "mark_to_move" ^ (string_of_int i) in
    let tg_trm = Path.resolve_path p t in
    let aux (tl : trm mlist) : unit =
      Mlist.iteri( fun i1 t1 ->
        match t1.desc with
        | Trm_for _ -> ()
        | _ -> Marks_basic.add mark (target_of_path (p @ [Dir_seq_nth i1]))

      ) tl in
     begin match tg_trm.desc with
     | Trm_seq tl -> aux tl
     | _ -> fail tg_trm.loc "Loop.fusion_targets: expected a target pointin to a marked sequence or a labelled sequence"
     end;
     Instr.move_out [nbMulti; cMark mark];
     Marks.remove mark [nbMulti; cMark mark];
     Loop_basic.fusion_on_block ~keep_label [cMark "mark_seq"]
  )

(* [move_out ~upto tg]: expects the target [tg] to point at an instruction inside a for loop,
    then it will move that instruction outside the for loop that it belongs to.
    In case of nested loops the user can specify the index of the upmost loop before which
    the instructions is going to be moved to.*)
let move_out ?(upto : string = "") (tg : target) : unit =
  Internal.nobrace_remove_after( fun _ ->
  iter_on_targets (fun t exp ->
    let (_, p) = Path.index_in_surrounding_loop exp in
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

(* [move before after loop_to_move]: move one loop before or after another loop in
     a "sequence"(not in the context of Optitrust) of nested loops.
     [before] - a default argument given as empty string, if the user wants to move
     [loop_to_move]: before another loop then it should use this default argument with the
                     value the quoted loop index
     [after] - similar to [before] but now is the index of the loop after whom we want to move [loop_to_move]. *)
let move ?(before : target = []) ?(after : target = []) (loop_to_move : target) : unit =
  Trace.call (fun t ->
   let loop_to_move_path = resolve_target_exactly_one_with_stringreprs_available loop_to_move t in
   let loop_to_move_trm = Path.resolve_path loop_to_move_path t in
   let loop_to_move_nested_indices = Internal.get_loop_nest_indices loop_to_move_trm in
   let loop_to_move_index  = List.nth loop_to_move_nested_indices 0 in
   begin match before, after with
   | [], [] -> fail None  "Loop.move: the before target or after target are mandatory please enter only one of them"
   | [], _ ->
    let targeted_loop_path = resolve_target_exactly_one_with_stringreprs_available after t in
    let targeted_loop = Path.resolve_path targeted_loop_path t in
    let targeted_loop_nested_indices = Internal.get_loop_nest_indices targeted_loop in
    let targeted_loop_index = List.nth targeted_loop_nested_indices  0 in
    if List.mem targeted_loop_index loop_to_move_nested_indices
      then begin
           let choped_indices = Xlist.chop_after targeted_loop_index loop_to_move_nested_indices in
           List.iter (fun _ -> Loop_basic.swap loop_to_move) choped_indices
           end
      else if List.mem loop_to_move_index targeted_loop_nested_indices then
        begin
        let choped_indices = Xlist.chop_after loop_to_move_index targeted_loop_nested_indices in
        let choped_indices = Xlist.chop_after targeted_loop_index (List.rev choped_indices) in
        let tg = target_of_path targeted_loop_path in
        List.iter (fun x -> Loop_basic.swap (tg @ [cFor x])) choped_indices
        end
      else fail loop_to_move_trm.loc "Loop.move: the given targets are not correct"

   | _ , [] ->
    let targeted_loop_path = resolve_target_exactly_one_with_stringreprs_available before t in
    let targeted_loop = Path.resolve_path targeted_loop_path t in
    let targeted_loop_nested_indices = Internal.get_loop_nest_indices targeted_loop in
    let targeted_loop_index = List.nth targeted_loop_nested_indices  0 in
    if List.mem targeted_loop_index loop_to_move_nested_indices
      then begin
           let choped_indices = Xlist.chop_after targeted_loop_index loop_to_move_nested_indices in
           let choped_indices = Xlist.chop_after loop_to_move_index (List.rev choped_indices) in
           List.iter (fun _ -> Loop_basic.swap loop_to_move) (List.rev choped_indices)
           end
      else if List.mem loop_to_move_index targeted_loop_nested_indices then
        begin
        let choped_indices = Xlist.chop_after loop_to_move_index targeted_loop_nested_indices in
        let tg = target_of_path targeted_loop_path in
        List.iter (fun x ->
          Loop_basic.swap (tg @ [cFor x]))
         (List.rev choped_indices)
        end
      else fail loop_to_move_trm.loc "Loop.move: the given targets are not correct"

   | _  -> fail None "Loop.move: only one of target before or after should be given"
   end
  )

(*
DETAILS for [unroll]

    Assumption: C should be a literal or a constant variable
    -------------------------------------------------------------------------------------------------------
    Ex:
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

    LATER: This transformation should be factorized, that may change the docs. *)

(* [unroll ~braces ~blocks ~shuffle tg]: expects the target to point at a loop. Then it checks if the loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the
    the instructions out of the loop and the loop will be removed. It works also
    in the case when C = 0 and a is a constant variable. To get the number of steps
    a is first inlined.

    [braces]: a flag on the visiblity of blocks created during the unroll process

    [blocks]: a list of integers describing the partition type of the targeted sequence

    [shuffle]: shuffle blocks  *)
let unroll ?(braces : bool = false) ?(blocks : int list = []) ?(shuffle : bool = false) (tg : target) : unit =
  reparse_after ~reparse:(not braces) (iteri_on_targets (fun i t p ->
    let my_mark = "__unroll_" ^ string_of_int i in
    let tg_loop_trm  = Path.resolve_path p t in
    Marks.add my_mark (target_of_path p);
    (* Function used in the case when the loop bound is a constant variable *)
    let aux (x : var) (t : trm) : int  =
      Variable_basic.unfold ~at:[cMark my_mark] [cVarDef x];
          let var_decl = match Internal.toplevel_decl x with
            | Some d -> d
            | None -> fail t.loc "Loop.unroll: could not find the declaration of the loop bound variable"
            in
          let lit_n = match get_init_val var_decl with
          | Some init1 -> init1
          | None -> fail t.loc "unroll: could not get the value of the loop component" in
          match (get_lit_from_trm_lit lit_n) with
          | Lit_int n -> n
          | _ -> fail t.loc "Loop.unroll: could not get the number of steps to unroll"
      in
    match tg_loop_trm.desc with
    | Trm_for (l_range, _) ->
      let (_, start, _, stop, _, _) = l_range in
      let nb_instr = begin match stop.desc with
      | Trm_apps (_, [_;bnd]) ->
        begin match bnd.desc with
        | Trm_val (Val_lit (Lit_int n)) -> n
        | Trm_var (_, x) -> aux x.qvar_var t
        | _ -> fail stop.loc "Loop.unroll: expected eitehr a constant variable of a literal"
        end
      | Trm_var (_, x) ->
          let start_nb = begin match start.desc with
          | Trm_var (_, y) -> aux y.qvar_var t
          | Trm_val (Val_lit (Lit_int n)) -> n
          | _ -> fail start.loc "Loop.unroll: expected a loop of the form for (int i = a; i < N; i where a should be a constant variable"
          end in
          (aux x.qvar_var t) - start_nb
      | Trm_val (Val_lit (Lit_int n)) -> n
      | _ -> fail stop.loc "Loop.unroll: expected an addition of two constants or a constant variable"
      end
        in
      Loop_basic.unroll ~braces:true ~my_mark [cMark my_mark];
      let block_list = Xlist.range 0 (nb_instr-1) in
      (* List.iter (fun x ->
        Variable.renames (AddSuffix (string_of_int x)) ([occIndex ~nb:nb_instr x; cMark my_mark;cSeq ()])
      ) block_list; *)
       List.iter (fun x ->
         Sequence_basic.partition ~braces blocks [cMark my_mark; dSeqNth x]
      ) block_list;
      if shuffle then Sequence_basic.shuffle ~braces [cMark my_mark];
      Marks.remove my_mark [nbAny;cMark my_mark]
    | _ -> fail tg_loop_trm.loc "Loop.unroll: expected a loop to unroll"
  )) tg

(* [reorder ~order tg]:  expects the target [tg] to point at the first loop included in the [order]
    list, then it will find all the nested loops starting from the targeted loop [tg] and
    reorder them based on [oder].

    Assumption:
      All loops have as bodies blocks of code(sequences).

    @correctness: correct if loops are parallelizable. *)
let reorder ?(order : vars = []) (tg : target) : unit =
  iter_on_targets (fun t p ->
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
    let _, targeted_loop_index = Xlist.unlast order in
    (*  LATER: use more precise targets, to avoid targeting deeply-nested loops that resue the same index *)
    List.iter (fun x -> move (target_of_path p @ [cFor x]) ~before:(target_of_path p @ [cFor targeted_loop_index])) order
  ) tg

(* internal *)
let fission_all_instrs_with_path_to_inner (nb_loops : int) (p : path) : unit =
  let rec aux (nb_loops : int) (p : path) : unit =
    if nb_loops > 0 then begin
      (* Apply fission to the next outer loop *)
      let p' = Path.to_outer_loop p in
      Loop_basic.fission_all_instrs (target_of_path p');
      (* And go through the remaining outer loops *)
      aux (nb_loops - 1) p';
    end
  in
  if nb_loops > 0 then begin
    (* Apply fission to the innermost loop *)
    Loop_basic.fission_all_instrs (target_of_path p);
    (* And go through the outer loops *)
    aux (nb_loops - 1) p
  end

let fission_all_instrs ?(nb_loops : int  = 1) (tg : target) : unit =
  Target.iter (fun t p ->
    Printf.printf "fission_all_instrs: %s\n" (Path.path_to_string p);
    (* apply fission helper on inner loop *)
    fission_all_instrs_with_path_to_inner nb_loops
      (Path.to_inner_loop_n (nb_loops - 1) p)
  ) tg

let fission ?(nb_loops : int  = 1) (tg : target) : unit =
  Target.iter (fun t p_before ->
    if nb_loops > 0 then begin
      Loop_basic.fission (target_of_path p_before);
      let (p_seq, _) = Path.last_dir_before_inv_success p_before in
      let p_loop = Path.parent_with_dir p_seq Dir_body in
      let p_outer_loop = Path.to_outer_loop p_loop in
      fission_all_instrs_with_path_to_inner (nb_loops - 1) p_outer_loop
    end
  ) tg

(* [fold ~index ~start ~sstep ~nb_instr tg]: similar to [Loop_basic.fold] (see loop_basic.ml) except that
    this one doesn't ask the user to prepare the sequence of instructions. But asks for the first instructions and
    the number of consecutive instructions [nb_instr] that can be converted into a single loop.
   @correctness: always correct, as we can map all intermediate predicates
   to numbered predicates on the loop. *)
let fold  ?(start : int = 0) ?(step : int = 1) ~index:(index : var) (nb_instr : int) (tg : target) : unit =
  let mark = "opti_fold" in
  Sequence_basic.intro ~mark nb_instr tg;
  Loop_basic.fold ~index ~start ~step [cMark mark]


(* [fold_instrs ~index ~start ~step tg]: similar to [fold] except that this one asks the user to provide a generic target
     that can match all the instructions that can be converted into a single loop. *)
let fold_instrs ~index:(index : var) ?(start : int = 0) ?(step : int = 1) (tg : target) : unit =
  let nb_targets = ref 0 in
  let prev_index = ref (-1) in
  let first_target = [occFirst] @ (filter_constr_occurrence tg) in
  let tg = enable_multi_targets tg in
  iter_on_targets (fun t p ->
      let _, i = Internal.isolate_last_dir_in_seq p in
      if i <> !prev_index + 1 && !prev_index <> -1 then fail t.loc "Loop.fold_instrs: all the targeted instructions should be consecutive ones";
      incr nb_targets;
    ) tg;
  if !nb_targets < 1 then fail None "Loop.fold_instrs: expected at least 1 instruction";
  fold ~index ~start ~step !nb_targets first_target;
  Variable.fold ~nonconst:true [nbAny;cVarDef "" ~body:[cInt !nb_targets]]

(* [isolate_first_iteration tg]: expects the target [tg] to be pointing at a simple loop, then it will
   split that loop into two loops by calling split_range transformation. Finally it will unroll the first loop. *)
let isolate_first_iteration (tg : target) : unit =
  Loop_basic.split_range ~nb:1 tg;
  unroll ([occFirst] @ tg)


(* [unfold_bound tg]: inlines the bound of the targeted loop if that loop is a simple for loop and if that bound
    is a variable and not a complex expression. *)
let unfold_bound (tg : target) : unit =
  iter_on_targets( fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_for (l_range, _) ->
      let (_, _, _, stop, _, _) = l_range in
      begin match stop.desc with
      | Trm_var (_, x) ->
        Variable_basic.unfold ~at:(target_of_path p) [cVarDef x.qvar_var]
      | Trm_apps (_, [{desc = Trm_var (_, x);_}]) when is_get_operation stop ->
        Variable_basic.unfold ~at:(target_of_path p) [cVarDef x.qvar_var]
      | _ -> fail tg_trm.loc "Loop.unfold_bound: can't unfold loop bounds that are not variables"
      end
    | _ -> fail tg_trm.loc "Loop.unfold_bound: expected a target to a simple for loop"
  ) tg

(* [grid_enumerate  ~indices tg]: similar to [Loop_basic.grid_enumerate](see loop_basic.ml) but this one computes
     the bounds automatically under the assumption that the bound of the targeted loop is given as a product of
    the bounds for each dimension. *)
let grid_enumerate ?(indices : string list = []) : Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_for (l_range, _) ->
      let (index, _, _, stop, _, _) = l_range in
      begin match trm_prod_inv stop with
      | [] -> fail tg_trm.loc "Loop.grid_enumerate: the bound of the targeted loop should be a product of the bounds of each dimension"
      | bounds ->
        let indices_and_bounds =
        if indices = [] then
          let indices = List.mapi (fun i _ -> index ^ (string_of_int i)) bounds in
          List.combine indices bounds
        else begin
          if List.length indices <> List.length bounds then fail tg_trm.loc "Loop.grid_enumerate: the provided list of indices does
            not correspond to the shape of the targeted loop bound";
          List.combine indices bounds
          end
          in
        Loop_basic.grid_enumerate indices_and_bounds (target_of_path p)
      end
    | _ -> fail tg_trm.loc "Loop.grid_enumerate: expected a target to a simple loop"
  )

(* [change_iter iterator_function main_loop_function tg]:  TODO ARTHUR spec *)
let change_iter ~src:(it_fun : var) ~dst:(loop_fun : var) (tg : target) : unit =
  iter_on_transformed_targets (Internal.isolate_last_dir_in_seq)
  (fun t (p, i) ->
    let tg_instr = target_of_path (p @ [Path.Dir_seq_nth i]) in
    (* First mark the top level function that contains the target tg *)
    let mark = "loop_change_iter_mark" in
    Marks.add mark (target_of_path p);
    let mark_tg = cMark mark in
    Function.uninline ~contains_for_loop:true ~fct:[cTopFunDef it_fun] tg_instr;
    Expr.replace_fun ~inline:true loop_fun [mark_tg; cFun it_fun];
    Function.beta ~indepth:true [mark_tg];
    Marks.remove mark [cMark mark]
  ) tg


(* TODO: what if index name is same as original loop index name? *)
let shift_aux (index : var) (inline : bool) (debug_name : string)
              (do_shift : string -> target -> unit) (tg : target) : unit =
  let index' = if index = "" then begin
    if not inline then fail None
      (debug_name ^ ": expected name for index variable when inline = false");
    Tools.next_tmp_name ();
  end else
    index
  in
  Target.iter (fun t p ->
    let tg_trm = Path.resolve_path p t in
    let error = debug_name ^ ": expected target to be a simple loop" in
    let ((prev_index, _, _, _, _, _), _) = trm_inv ~error trm_for_inv tg_trm in begin
    do_shift index' (target_of_path p);
    Arith_basic.(simpl gather) (target_of_path (p @ [Dir_for_start]));
    Arith_basic.(simpl gather) (target_of_path (p @ [Dir_for_stop]));
    if inline then begin
      let mark = Mark.next() in
      let  _ = Variable_basic.inline ~mark (target_of_path (p @ [Dir_body; Dir_seq_nth 0])) in
      Arith.(simpl_surrounding_expr gather) [nbAny; cMark mark]
    end;
    if index = "" then
      Loop_basic.rename_index prev_index (target_of_path p)
    end
  ) tg

(* [shift ~index amount ~inline]: shifts a loop index by a given amount.
   - [inline] if true, inline the index shift in the loop body *)
let shift ?(reparse : bool = false) ?(index : var = "") (amount : trm) ?(inline : bool = true) (tg : target) : unit =
  shift_aux index inline "Loop.shift" (fun i tg -> Loop_basic.shift ~reparse i amount tg) tg

(* [shift_to_zero index ~inline]: shifts a loop index to start from zero.
    - [inline] if true, inline the index shift in the loop body *)
let shift_to_zero ?(reparse : bool = false)
                  ?(index : var = "")
                  ?(inline : bool = true)
                  (tg : target) : unit =
  shift_aux index inline "Loop.shift_to_zero" (Loop_basic.shift_to_zero ~reparse) tg

(* should the nested loop iterate over:
   - [TileIterLocal] local tile indices? (loops are easy to swap)
   - [TileIterGlobal] global loop indices? *)
type tile_iteration = TileIterLocal | TileIterGlobal

let tile ?(index : var = "b${id}")
        ?(bound : tile_bound = TileBoundMin)
        ?(iter : tile_iteration = TileIterLocal)
        (tile_size : trm) : Transfo.t =
  Target.iter (fun t p ->
    match iter with
    | TileIterLocal -> begin
      reparse_after (Loop_basic.tile ~index ~bound tile_size) (target_of_path p);
      shift_to_zero (target_of_path (Path.to_inner_loop p));
    end
    | TileIterGlobal ->
      Loop_basic.tile ~index ~bound tile_size (target_of_path p)
  )
   