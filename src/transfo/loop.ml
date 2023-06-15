open Ast
open Target
include Loop_basic

let default_simpl = Arith.default_simpl

(* [rename]: instantiation of Rename module *)
type rename = Variable.Rename.t

let path_of_loop_surrounding_mark_current_ast (m : mark) : path =
  let mark_path = path_of_target_mark_one_current_ast m in
  let (_, loop_path) = Path.index_in_surrounding_loop mark_path in
  loop_path

(* LATER/ deprecated *)
let hoist_old ?(name : var = "${var}_step") ?(array_size : trm option) (tg : target) : unit =
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
          Loop_basic.hoist_old ~name ?array_size (target_of_path p);
        | false -> Loop_basic.hoist_old ~name ?array_size (target_of_path p)
  ) tg

(* TODO: redundant with 'hoist' *)
(* [hoist_alloc_loop_list]: this transformation is similar to [Loop_basic.hoist], but also supports undetached
   variable declarations, hoisting through multiple loops, and inlining array indexing code.
    [tmp_names] - pattern used to generate temporary names
    [name] - name of the hoisted matrix
    [inline] - inlines the array indexing code
    [loops] - loops to hoist through (expecting a perfect nest of simple loops),
              where [0] represents a loop for which no dimension should be created,
              and [1] represents a loop for which a dimension should be created.
  *)
let%transfo hoist_alloc_loop_list
  ?(tmp_names : string = "${var}_step${i}")
  ?(name : string = "")
  ?(inline : bool = true)
  (loops : int list)
  (tg : target) : unit
  =
  Trace.step_valid_by_composition ();
  let tmp_marks = ref [] in
  let alloc_mark = Mark.next () in
  let may_detach_init (x : var) (init : trm) (p : path) =
    let is_trm_malloc = Option.is_some (Matrix_core.alloc_inv_with_ty init) in
    let detach_first = not (is_trm_malloc || (is_trm_uninitialized init) || (is_trm_new_uninitialized init)) in
    if detach_first then begin
      Variable_basic.init_detach (target_of_path p);
      let (_, seq_path) = Path.index_in_seq p in
      Matrix_basic.intro_malloc0 x (target_of_path seq_path);
    end
  in
  let rec mark_and_hoist prev_name name_template (i : int) (p : path) =
    let more_hoists = i + 1 <= (List.length loops) in
    let maybe_mark = if more_hoists then None else Some alloc_mark in
    let varies_in_current_loop = List.nth loops ((List.length loops) - i) in
    let next_name = match varies_in_current_loop with
    | 0 -> begin
      (* Printf.printf "move out %s\n" prev_name; *)
      (* TODO: have combined move_out alloc + free *)
      Loop_basic.move_out ?mark:maybe_mark (target_of_path p);
      let (outer_i, outer_path) = Path.index_in_seq (Path.to_outer_loop p) in
      let new_loop_path = outer_path @ [Dir_seq_nth (outer_i + 1)] in
      Instr.move ~dest:([tAfter] @ (target_of_path new_loop_path)) [cFun ~args:[[cVar prev_name]] "free"];
      prev_name
    end
    | 1 -> begin
      let next_name = Tools.string_subst "${i}" (string_of_int i) name_template in
      (* Printf.printf "hoist %s -> %s\n" prev_name next_name; *)
      let (instr_index, seq_path) = Path.index_in_seq p in
      let seq_target = target_of_path seq_path in
      let seq_mark = Mark.next () in
      Marks.add seq_mark seq_target;
      tmp_marks := (seq_mark, instr_index) :: !tmp_marks;

      Loop_basic.hoist ~name:next_name ?mark:maybe_mark (target_of_path p);
      next_name
      end
    | _ -> fail None "expected list of 0 and 1s"
    in
    let (_, loop_path) = Path.index_in_surrounding_loop p in
    let next_target = (target_of_path loop_path) @ [cVarDef next_name] in
    if more_hoists then
      iter_on_targets (fun t p -> mark_and_hoist next_name name_template (i + 1) p) next_target;
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
          let instr_path = loop_p @ [Dir_seq_nth instr_index] in
          let instr_target = target_of_path instr_path in
          (* DEPRECATED since MALLOC0/MINDEX0
          let instr = Path.resolve_path instr_path t in
          let is_partial_mindex = not (trm_has_cstyle Reference instr) in
          if is_partial_mindex then begin
          *)
            Variable_basic.to_const instr_target;
            Marks.with_fresh_mark (fun mark ->
              Variable_basic.inline ~mark instr_target;
              simpl_index_after_inline [nbAny; cMark mark]
            )
          (* end else
            Variable_basic.inline instr_target; *)
        ) [cMark seq_mark];
        (* FIXME if not inline, we have MINDEX0 artifacts *)
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
      if 1 <= (List.length loops) then begin
        let name_template = Tools.string_subst "${var}" x tmp_names in
        let alloc_name =
          if inline && (name = "")
          then x
          else Tools.string_subst "${var}" x name
        in
        may_detach_init x init p;
        mark_and_hoist x name_template 1 p;
        make_pretty_and_unmark alloc_name;
      end
    | _ -> fail tg_trm.loc "Loop.hoist: expected a variable declaration"
  ) tg

(* [hoist ~name ~array_size ~inline tg]: this transformation is similar to [Loop_basic.hoist] (see loop_basic.ml) except that this
    transformation supports also undetached declarations as well as hoisting through multiple loops.
    [inline] - inlines the array indexing code
    [nest_of] - number of loops to hoist through (expecting a perfect nest of simple loops)

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
let%transfo hoist ?(tmp_names : string = "${var}_step${i}")
          ?(name : string = "")
          ?(inline : bool = true)
          ?(nest_of : int = 1)
          (tg : target) : unit =
  hoist_alloc_loop_list ~tmp_names ~name ~inline (List.init nest_of (fun _ -> 1)) tg

(* [hoist_instr_loop_list]: this transformation hoists an instructions outside of multiple loops using a combination of
  [Loop_basic.move_out], [Instr_basic.move], and [Loop_basic.fission].
  [loops] - loops to hoist through (expecting a perfect nest of simple loops),
            where [0] represents a loop for which no dimension should be created,
            and [1] represents a loop for which a dimension should be created.
*)
let%transfo hoist_instr_loop_list (loops : int list) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  let rec aux (remaining_loops : int list) (p : path) : unit =
    match remaining_loops with
    | [] -> ()
    | 0 :: rl ->
      Marks.with_fresh_mark (fun m ->
        Loop_basic.move_out ~mark:m (target_of_path p);
        iter_on_targets (fun t p -> aux rl p) [cMark m];
      )
    | 1 :: rl ->
      let (idx, loop_path) = Path.index_in_surrounding_loop p in
      let loop_target = target_of_path loop_path in
      if idx > 0 then
        Instr_basic.move ~dest:(loop_target @ [tFirst; dBody]) (target_of_path p);
      Loop_basic.fission (loop_target @ [tAfter; dBody; dSeqNth 0]);
      aux rl loop_path;
    | _ -> fail None "expected list of 0 and 1s"
  in
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    assert (Option.is_none (trm_let_inv tg_trm));
    aux (List.rev loops) p;
  ) tg

(* [hoist_decl_loop_list]: this transformation hoists a variable declaration outside of multiple loops
   using a combination of [hoist_alloc_loop_list] for the allocation and [hoist_instr_loop_list] for the initialization. *)
let%transfo hoist_decl_loop_list
  ?(tmp_names : string = "${var}_step${i}")
  ?(name : string = "")
  ?(inline : bool = true)
  (loops : int list)
  (tg : target) : unit
  =
  Trace.step_valid_by_composition ();
  Target.iter (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match trm_let_inv tg_trm with
    | Some (_, x, _, init) -> Marks.with_fresh_mark_on (p @ [Dir_body; Dir_arg_nth 0]) (fun m ->
        hoist_alloc_loop_list ~tmp_names ~name ~inline loops tg;
        hoist_instr_loop_list loops [cBinop ~rhs:[cMark m] Binop_set];
      )
    | None -> fail tg_trm.loc "expected let"
  ) tg

(* private *)
let find_surrounding_instr (p : path) (t : trm) : path =
  let rec aux p =
    let p_t = Path.resolve_path p t in
    if p_t.is_statement then p else aux (Path.parent p)
  in
  assert (not (Path.resolve_path p t).is_statement);
  aux p

(* [hoist_expr_loop_list]: this transformation hoists an expression outside of multiple loops
   using a combination of [Variable.bind] to create a variable and [hoist_decl_loop_list] to hoist the variable declaration. *)
let%transfo hoist_expr_loop_list (name : string)
                         (loops : int list)
                         (tg : target) : unit =
  Trace.step_valid_by_composition ();
  Target.iter (fun t p ->
    let instr_path = find_surrounding_instr p t in
    Variable.bind name (target_of_path p);
    hoist_decl_loop_list loops (target_of_path instr_path);
  ) tg

(* internal *)
let targets_iter_with_loop_lists
  ?(indep : var list = [])
  ?(dest : target = [])
  (f : int list -> path -> unit)
  (tg : target) : unit =
begin
  assert (dest <> []);
  Target.iter (fun t target_path ->
    let hoist_path = Target.resolve_target_exactly_one dest t in
    let (common_path, hoist_relpath, target_relpath) = Path.split_common_prefix hoist_path target_path in
    (*
    Printf.printf "common path: %s\n" (Path.path_to_string common_path);
    Printf.printf "hoist relative path: %s\n" (Path.path_to_string hoist_relpath);
    Printf.printf "target relative path: %s\n" (Path.path_to_string target_relpath);
    *)
    let hoist_before_index = match hoist_relpath with
    | Dir_before bi :: [] -> bi
    | _ -> fail None "Loop.targets_iter_with_loop_lists expects [before] to point a sequence surrounding its target"
    in
    (* TODO: otherwise, need to move instrs after hoist. *)
    assert ((List.hd target_relpath) = (Dir_seq_nth hoist_before_index));
    let (rev_loop_list, _) = List.fold_left (fun (rev_loop_list, p) elem ->
      let new_rev_loop_list = match trm_for_inv (resolve_path_current_ast p) with
      | Some ((i, _start, _dir, _stop, _step, _par), _) ->
        let loop_val = if List.mem i indep then 0 else 1 in
        loop_val :: rev_loop_list
      | None -> rev_loop_list
      in
      (new_rev_loop_list, p @ [elem])
    ) ([], common_path) (Path.parent target_relpath)
    in
    f (List.rev rev_loop_list) target_path
  ) tg
end

(* TODO: add unit tests in combi/loop_hoist.ml *)
(* [hoist_expr]: same as [hoist_expr_loop_list], but allows specifying
   loop indices that the expression does not depend on in [indep],
   and specifying where to hoist using [dest] target. *)
let%transfo hoist_expr (name : string)
               ?(indep : var list = [])
               ?(dest : target = [])
               (tg : target) : unit =
  Trace.step_valid_by_composition ();
  targets_iter_with_loop_lists ~indep ~dest (fun loops p ->
    (* Printf.printf "%s\n" (Tools.list_to_string (List.map string_of_int loops)); *)
    hoist_expr_loop_list name loops (target_of_path p)
  ) tg

let%transfo simpl_range ~(simpl : Target.Transfo.t) (tg : target) : unit =
  Trace.step_simpl_arith ();
  Target.iter (fun _ p ->
    simpl (target_of_path (p @ [Dir_for_start]));
    simpl (target_of_path (p @ [Dir_for_stop]));
    simpl (nbAny :: (target_of_path (p @ [Dir_for_step])));
  ) tg

(* [shift ~index kind ~inline]: shifts a loop index according to [kind].
- [inline] if true, inline the index shift in the loop body *)
(* TODO: what if index name is same as original loop index name? *)
let%transfo shift ?(reparse : bool = false) ?(index : var = "") (kind : shift_kind) ?(inline : bool = true) ?(simpl: Transfo.t = default_simpl) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  let index' = if index = "" then begin
    if not inline then
      fail None "Loop.shift: expected name for index variable when inline = false";
    Tools.next_tmp_name ();
  end else
    index
  in
  Target.iter (fun t p ->
  let tg_trm = Path.resolve_path p t in
  let error = "Loop.shift: expected target to be a simple loop" in
  let ((prev_index, _, _, _, _, _), _) = trm_inv ~error trm_for_inv tg_trm in begin
  Loop_basic.shift index' kind (target_of_path p);
  simpl_range ~simpl (target_of_path p);
  if inline then begin
    let mark = Mark.next() in
    let  _ = Variable_basic.inline ~mark (target_of_path (p @ [Dir_body; Dir_seq_nth 0])) in
    simpl [nbAny; cMark mark]
  end;
  if index = "" then
  Loop_basic.rename_index prev_index (target_of_path p)
  end
  ) tg

(* [extend_range]: like [Loop_basic.extend_range], plus arithmetic and conditional simplifications.
   *)
let%transfo extend_range ?(start : extension_kind = ExtendNothing) ?(stop : extension_kind = ExtendNothing) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  Target.iter (fun t p ->
    Loop_basic.extend_range ~start ~stop (target_of_path p);
    (* TODO: simpl_range? *)
    simpl (target_of_path (p @ [Dir_for_start]));
    simpl (target_of_path (p @ [Dir_for_stop]));
    let tg_loop = Target.resolve_path_current_ast p in
    let (_, loop_instrs) = trm_inv ~error:"Loop.extend_range: expected simple loop"
      trm_for_inv_instrs tg_loop in
    match Mlist.to_list loop_instrs with
    | [single_intsr] ->
      (* TODO: simplify conditions such as:
          start <= index,
          index < stop
         *)
      if Option.is_some (trm_if_inv single_intsr) then begin
        simpl (target_of_path (p @ [Dir_body; Dir_seq_nth 0; Dir_cond]));
      end
    | _ -> ()
  ) tg

(* internal *)
(* TODO: doc *)
let adapt_indices ~(upwards : bool) (p : path) : unit =
  let t = Trace.ast () in
  let (index, p_seq) = Path.index_in_seq p in
  let (loop1_p, loop2_p) =
    if upwards
    then (p, p_seq @ [Dir_seq_nth (index + 1)])
    else (p, p_seq @ [Dir_seq_nth (index - 1)])
  in
  let loop1 = Path.resolve_path loop1_p t in
  let loop2 = Path.resolve_path loop2_p t in
  let error = "expected simple loop" in
  let (loop_range1, _) = trm_inv ~error trm_for_inv loop1 in
  let (loop_range2, _) = trm_inv ~error trm_for_inv loop2 in
  if not (same_loop_index loop_range1 loop_range2) then begin
    let (idx, _, _, _, _, _) = loop_range1 in
    rename_index idx (target_of_path loop2_p)
  end

(* [fusion nb tg]: expects the target [tg] to point at a for loop followed by one or more for loops.
    Merge them into a single loop.

    [nb] - denotes the number of sequenced loops to consider.
    [nest_of] - denotes the number of nested loops to consider.
    [adapt_fused_indices] - attempts to adapt the indices of fused loops using [Loop.extend_range] and [Loop.shift], otherwise by default the loops need to have the same range.
  *)
let%transfo fusion ?(nb : int = 2) ?(nest_of : int = 1) ?(upwards : bool = true) ?(adapt_fused_indices : bool = true) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  Target.iter (fun _ p ->
    Marks.with_fresh_mark_on p (fun m ->
      for _ = 2 to nb do
        for nest_id = 0 to (nest_of - 1) do
          let cur_p = Target.path_of_target_mark_one_current_ast m in
          let nested_p = Path.to_inner_loop_n nest_id cur_p in
          let target_p = if upwards || nest_id = 0
          then nested_p
          else
            let (i, p) = Path.index_in_seq nested_p in
            p @ [Path.Dir_seq_nth (i + 1)]
          in
          if adapt_fused_indices then
            adapt_indices ~upwards target_p;
          Loop_basic.fusion ~upwards (target_of_path target_p);
        done
      done
    )
  ) tg

(* [fusion_targets tg]: similar to [fusion] except that this transformation assumes that [tg] points to multiple
    not neccessarily consecutive for loops.
    All targeted loops must be in the same sequence.

  [into] - Specifies into which loop to fuse all other.
    Otherwise, fuse into the first loop in the sequence.
  [nest_of] - denotes the number of nested loops to consider.

  LATER ?(into_occ : int = 1)
  *)
let%transfo fusion_targets ?(into : target option) ?(nest_of : int = 1) ?(adapt_all_indices : bool = false) ?(adapt_fused_indices : bool = true) ?(rename : path -> Variable.rename option = fun p -> None) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  assert (not adapt_all_indices); (* TODO *)
  (* adapt_all_indices => adapt_fused_indices *)
  let adapt_fused_indices = adapt_all_indices || adapt_fused_indices in
  (* First, retrieve the paths to all loops,
     checking that all paths are in the same sequence,
     and remembering the indices of the targeted loops in this sequence. *)
  let seq_path = ref None in
  let indices_in_seq = ref [] in
  Target.iter (fun t p ->
    let (i, p_seq) = Path.index_in_seq p in
    begin match !seq_path with
    | None -> seq_path := Some p_seq
    | Some p_seq' ->
      if p_seq <> p_seq' then
        fail t.loc "Loop.fusion_targets: targeted loops are not in the same sequence"
    end;
    indices_in_seq := i :: !indices_in_seq;
  ) (nbMulti :: tg);
  (* TODO: use gather_targets GatherAt preprocessing *)
  (* Then, fuse all loops into one, moving loops in the sequence if necessary. *)
  let may_rename_loop_body loop_p =
    Option.iter  (fun r ->
      let inner_loop_p = Path.to_inner_loop_n (nest_of - 1) loop_p in
      Variable.renames r (target_of_path (inner_loop_p @ [Path.Dir_body]));
    ) (rename loop_p)
    in
  let p_seq = Option.get !seq_path in
  let ordered_indices = List.sort compare !indices_in_seq in
  let rec fuse_loops fuse_into shift todo =
    match todo with
    | [] -> ()
    | to_fuse :: todo ->
      let fuse_into_tg = target_of_path (p_seq @ [Path.Dir_seq_nth fuse_into]) in
      (* If we are fusing from top to bottom *)
      if to_fuse < fuse_into then begin
        (* Printf.printf "to_fuse: %i\n" to_fuse;
        Printf.printf "fuse_into: %i\n" fuse_into; *)
        let to_fuse' = to_fuse in (* no shift *)
        let p_current = p_seq @ [Path.Dir_seq_nth to_fuse'] in
        may_rename_loop_body p_current;
        if to_fuse' <> fuse_into - 1 then begin
          Instr_basic.move ~dest:(tBefore :: fuse_into_tg) (target_of_path p_current);
        end;
        fusion ~nest_of ~adapt_fused_indices ~upwards:false fuse_into_tg;
        fuse_loops (fuse_into - 1) (shift - 1) todo;
      end;
      (* If we are fusing from bottom to top *)
      if to_fuse > fuse_into then begin
        let to_fuse' = to_fuse + shift in
        let p_current = p_seq @ [Path.Dir_seq_nth to_fuse'] in
        may_rename_loop_body p_current;
        if to_fuse' <> (fuse_into + 1) then begin
          Instr_basic.move ~dest:(tAfter :: fuse_into_tg) (target_of_path p_current);
        end;
        fusion ~nest_of ~adapt_fused_indices fuse_into_tg;
        fuse_loops fuse_into (shift - 1) todo;
      end;
  in
  match into with
  | Some tg ->
    let p = Target.resolve_target_exactly_one tg (Trace.ast ()) in
    may_rename_loop_body p;
    let (fuse_into, p_seq) = Path.index_in_seq p in
    (* TODO: Option.get error message *)
    let pos = Option.get (Xlist.index_of fuse_into ordered_indices) in
    let (before, inc_after) = Xlist.split_at pos ordered_indices in
    let after = Xlist.drop 1 inc_after in
    let to_fuse = (List.rev before) @ after in
    (* Printf.printf "fuse_into: %i\n" fuse_into;
    List.iter (Printf.printf "%i ") to_fuse;
    Printf.printf "\n"; *)
    fuse_loops fuse_into 0 to_fuse
  | None ->
    match ordered_indices with
    | [] -> ()
    | fuse_into :: to_fuse ->
      may_rename_loop_body (p_seq @ [Dir_seq_nth fuse_into]);
      fuse_loops fuse_into 0 to_fuse

(* [move_out ~upto tg]: expects the target [tg] to point at an instruction inside a for loop,
    then it will move that instruction outside the for loop that it belongs to.
    In case of nested loops the user can specify the index of the upmost loop before which
    the instructions is going to be moved to.*)
let%transfo move_out ?(upto : string = "") (tg : target) : unit =
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
let%transfo move ?(before : target = []) ?(after : target = []) (loop_to_move : target) : unit =
  Trace.step_valid_by_composition ();
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

let%transfo unroll_nest_of_1 ?(braces : bool = false) ?(blocks : int list = []) ?(shuffle : bool = false) (tg : target) : unit =
  (* in [unroll]: reparse_after ~reparse:(not braces) *)
  Target.iteri (fun i t p ->
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
  ) tg

(* [unroll ~braces ~blocks ~shuffle tg]: expects the target to point at a loop. Then it checks if the loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the
    the instructions out of the loop and the loop will be removed. It works also
    in the case when C = 0 and a is a constant variable. To get the number of steps
    a is first inlined.

    [braces]: a flag on the visiblity of blocks created during the unroll process

    [blocks]: a list of integers describing the partition type of the targeted sequence

    [shuffle]: shuffle blocks

    [nest_of]: denotes the number of nested loops to consider. *)
let%transfo unroll ?(braces : bool = false) ?(blocks : int list = []) ?(shuffle : bool = false) ?(nest_of : int = 1) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  assert (nest_of > 0);
  let rec aux p nest_of =
    if nest_of > 1 then
      aux (Path.to_inner_loop p) (nest_of - 1);
    unroll_nest_of_1 (target_of_path p);
  in
  (* reparse_after ~reparse:(not braces) ( *)
    Target.iter (fun t p -> aux p nest_of)
  (* ) *) tg

(* [reorder ~order tg]:  expects the target [tg] to point at the first loop included in the [order]
    list, then it will find all the nested loops starting from the targeted loop [tg] and
    reorder them based on [oder].

    Assumption:
      All loops have as bodies blocks of code(sequences).

    @correctness: correct if loops are parallelizable. *)
let%transfo reorder ?(order : vars = []) (tg : target) : unit =
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

(* [bring_down_loop]: given a path [p] to an instruction, find a surrounding
   loop over [index] and bring it down to immediately surround the instruction. In order to swap imperfect loop nests,
   local variables will be hoisted ([Loop.hoist]),
   and surrounding instructions will be fissioned ([Loop.fission_all_instrs]).
   *)
let rec bring_down_loop ?(is_at_bottom : bool = true) (index : var) (p : path): unit =
  let hoist_all_allocs (tg : target) : unit =
    hoist_alloc_loop_list [1] (tg @ [nbAny; cStrict; cVarDef ""])
  in
  (* with_fresh_mark_on_p *)
  Marks.with_fresh_mark_on p (fun m ->
    let (_, loop_path) = Path.index_in_surrounding_loop p in
    let loop_trm = Path.resolve_path loop_path (Trace.ast ()) in
    let ((i, _start, _dir, _stop, _step, _par), body) = trm_inv
      ~error:"Loop.reorder_at: expected simple loop."
      trm_for_inv loop_trm in
    (* Printf.printf "before i = '%s':\n%s\n" i (AstC_to_c.ast_to_string (Trace.ast ())); *)
    (* recursively bring the loop down if necessary *)
    if i <> index then
      bring_down_loop index ~is_at_bottom:false loop_path;
    (* bring the loop down by one if necessary *)
    if not is_at_bottom then begin
      (* hoist all allocs and fission all instrs to isolate the
          loops to be swaped *)
      hoist_all_allocs (target_of_path (path_of_loop_surrounding_mark_current_ast m));
      fission_all_instrs (target_of_path (path_of_loop_surrounding_mark_current_ast m));
      Loop_basic.swap (target_of_path (path_of_loop_surrounding_mark_current_ast m));
      (* Printf.printf "after i = '%s':\n%s\n" i (AstC_to_c.ast_to_string (Trace.ast ())); *)
    end
  )

(* [reorder_at ~order tg]: expects the target [tg] to point at an instruction that is surrounded
   by [length order] loops, and attempts to reorder these loops according to [order].
   The loops do not have to be perfectly nested. In order to swap imperfect loop nests,
   local variables will be hoisted ([Loop.hoist]),
   and surrounding instructions will be fissioned ([Loop.fission_all_instrs]).
   *)
let%transfo reorder_at ?(order : vars = []) (tg : target) : unit =
  Trace.step_valid_by_composition ();
  (* [remaining_loops]: sublist of [List.rev order]
     [p]: path to either the target instruction at [tg],
          or a surrounding for loop. *)
  let rec aux (remaining_loops : var list) (p : path) : unit =
    match remaining_loops with
    | [] -> ()
    | loop_index :: rl -> begin
      (* Printf.printf "index = '%s'\n" index; *)
      Marks.with_fresh_mark_on p (fun m ->
        bring_down_loop loop_index p;
        aux rl (path_of_loop_surrounding_mark_current_ast m)
      )
      end
  in
  Target.iter (fun t p ->
    aux (List.rev order) p
  ) tg

(* internal *)
let fission_all_instrs_with_path_to_inner (nest_of : int) (p : path) : unit =
  let rec aux (nest_of : int) (p : path) : unit =
    if nest_of > 0 then begin
      (* Apply fission to the next outer loop *)
      let p' = Path.to_outer_loop p in
      Loop_basic.fission_all_instrs (target_of_path p');
      (* And go through the remaining outer loops *)
      aux (nest_of - 1) p';
    end
  in
  if nest_of > 0 then begin
    (* Apply fission to the innermost loop *)
    Loop_basic.fission_all_instrs (target_of_path p);
    (* And go through the outer loops *)
    aux (nest_of - 1) p
  end

let%transfo fission_all_instrs ?(nest_of : int  = 1) (tg : target) : unit =
  Target.iter (fun t p ->
    (* Printf.printf "fission_all_instrs: %s\n" (Path.path_to_string p); *)
    (* apply fission helper on inner loop *)
    fission_all_instrs_with_path_to_inner nest_of
      (Path.to_inner_loop_n (nest_of - 1) p)
  ) tg

let%transfo fission ?(nest_of : int  = 1) (tg : target) : unit =
  Target.iter (fun t p_before ->
    if nest_of > 0 then begin
      Loop_basic.fission (target_of_path p_before);
      if nest_of > 1 then begin
        let (p_seq, _) = Path.last_dir_before_inv_success p_before in
        let p_loop = Path.parent_with_dir p_seq Dir_body in
        let p_outer_loop = Path.to_outer_loop p_loop in
        fission_all_instrs_with_path_to_inner (nest_of - 1) p_outer_loop
      end
    end
  ) tg

(* [fold ~index ~start ~sstep ~nb_instr tg]: similar to [Loop_basic.fold] (see loop_basic.ml) except that
    this one doesn't ask the user to prepare the sequence of instructions. But asks for the first instructions and
    the number of consecutive instructions [nb_instr] that can be converted into a single loop.
   @correctness: always correct, as we can map all intermediate predicates
   to numbered predicates on the loop. *)
let%transfo fold  ?(start : int = 0) ?(step : int = 1) ~index:(index : var) (nb_instr : int) (tg : target) : unit =
  let mark = "opti_fold" in
  Sequence_basic.intro ~mark nb_instr tg;
  Loop_basic.fold ~index ~start ~step [cMark mark]


(* [fold_instrs ~index ~start ~step tg]: similar to [fold] except that this one asks the user to provide a generic target
     that can match all the instructions that can be converted into a single loop. *)
let%transfo fold_instrs ~index:(index : var) ?(start : int = 0) ?(step : int = 1) (tg : target) : unit =
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
let%transfo isolate_first_iteration (tg : target) : unit =
  Loop_basic.split_range ~nb:1 tg;
  unroll ([occFirst] @ tg)


(* [unfold_bound tg]: inlines the bound of the targeted loop if that loop is a simple for loop and if that bound
    is a variable and not a complex expression. *)
let%transfo unfold_bound (tg : target) : unit =
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
let%transfo change_iter ~src:(it_fun : var) ~dst:(loop_fun : var) (tg : target) : unit =
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

(* should the nested loop iterate over:
   - [TileIterLocal] local tile indices? (loops are easy to swap)
   - [TileIterGlobal] global loop indices? *)
type tile_iteration = TileIterLocal | TileIterGlobal

let tile_iteration_to_string = function
  | TileIterLocal -> "TileIterLocal"
  | TileIterGlobal -> "TileIterGlobal"

let%transfo tile ?(index : var = "b${id}")
        ?(bound : tile_bound = TileBoundMin)
        ?(iter : tile_iteration = TileIterLocal)
        (tile_size : trm)
        (tg : target) : unit =
  Trace.step_valid_by_composition ();
  Target.iter (fun t p ->
    match (iter, bound) with
    | (TileIterGlobal, _) | (_, TileDivides) ->
      Loop_basic.tile ~index ~bound tile_size (target_of_path p)
    | _ -> begin
      reparse_after (Loop_basic.tile ~index ~bound tile_size) (target_of_path p);
      shift StartAtZero (target_of_path (Path.to_inner_loop p));
    end
  ) tg

(* [slide]: like [tile] but with the addition of a [step] parameter that controls how many iterations stand between the start of two tiles. Depending on [step] and [size], some iterations may be discarded or duplicated.
*)
let%transfo slide ?(index : var = "b${id}")
  ?(bound : tile_bound = TileBoundMin)
  ?(iter : tile_iteration = TileIterLocal)
  ~(size : trm)
  ~(step : trm)
  ?(simpl : Transfo.t = default_simpl)
  (tg : target) : unit =
  Trace.step_valid_by_composition ();
  Target.iter (fun _ p ->
    let bound = if is_trm_int 1 step then TileDivides else bound in
    Loop_basic.slide ~index ~bound ~size ~step (target_of_path p);
    simpl_range ~simpl (target_of_path p);
    begin match iter with
    | TileIterLocal ->
      shift StartAtZero ~simpl (target_of_path (Path.to_inner_loop p));
    | _  ->
      simpl_range ~simpl (target_of_path (Path.to_inner_loop p));
    end;
  ) tg

(* [slides]: like [slide], but operating on a nest of multiple loops and putting all loops over elements inside the bunch of loops over tiles. *)
let%transfo slides ?(index : var = "b${id}")
  ?(bound : tile_bound = TileBoundMin)
  ?(iter : tile_iteration = TileIterLocal)
  ~(size_steps : (trm * trm) option list)
  ?(simpl : Transfo.t = default_simpl)
  (tg : target) : unit =
  Trace.step_valid_by_composition ();
  Target.iter (fun _ p ->
    let size_steps_bottom_up = List.rev (List.mapi (fun i x -> (i, x)) size_steps) in
    let prev_outer_elt_loop = ref None in
    let slide_at_inner_loop (i, size_step) =
      match size_step with
      | Some (size, step) ->
        let target_p = Path.to_inner_loop_n i p in
        (* Debug_transfo.current_ast_at_path "sliding" target_p; *)
        slide ~index ~bound ~iter ~size ~step ~simpl (target_of_path target_p);
        let tile_loop_p = Path.to_inner_loop target_p in
        begin match !prev_outer_elt_loop with
        | Some potl ->
          let outer_elt_loop = Path.to_inner_loop potl in
          move (target_of_path tile_loop_p) ~before:(target_of_path outer_elt_loop);
          prev_outer_elt_loop := Some (Path.to_outer_loop outer_elt_loop)
        | None ->
          prev_outer_elt_loop := Some (tile_loop_p)
        end
      | None -> ()
    in
    List.iter slide_at_inner_loop size_steps_bottom_up
  ) tg

(* [delete_void]: deletes a loop nest with empty body.

  [nest_of] - number of perfectly nested loops to delete
  *)
let%transfo delete_void ?(nest_of : int = 1) (tg : target) : unit =
  let rec aux (nest_of : int) (p : path) : unit =
    if nest_of > 0 then begin
      aux (nest_of - 1) (Path.to_inner_loop p);
      Loop_basic.delete_void (target_of_path p);
    end
  in
  Target.iter (fun _ p -> aux nest_of p) tg

(* TODO: should this be in basic? *)
(* [delete_void]: deletes all loop nests with empty body. *)
let%transfo delete_all_void (tg : target) : unit =
  Trace.step_justif_always_correct ();
  Target.apply (fun t p ->
    Path.apply_on_path (trm_bottom_up (fun t ->
      match trm_seq_inv t with
      | Some instrs ->
        let res_t = ref t in
        for i = (Mlist.length instrs) - 1 downto 0 do
          match Loop_basic.delete_void_on i !res_t with
          | Some t2 -> res_t := t2
          | None -> ()
        done;
        !res_t
      | None -> t
    )) t p
  ) tg

let rec get_indices (nest_of : int) (outer_p : path) : var list =
  if nest_of > 0 then
    let error = "Loop.get_indices: expected simple loop" in
    let ((index, _, _, _, _, _), _) = trm_inv ~error trm_for_inv (Path.resolve_path outer_p (Trace.ast ())) in
    let nested_indices = get_indices (nest_of - 1) (Path.to_inner_loop outer_p) in
    index :: nested_indices
  else []

(* sets loop indices, internal because there must be no overlap between new names and previous names *)
let rec set_indices_internal (indices : var list) (outer_p : path) : unit =
  match indices with
  | i :: ri ->
    Loop_basic.rename_index i (target_of_path outer_p);
    set_indices_internal ri (Path.to_inner_loop outer_p)
  | [] -> ()

let set_indices (indices : var list) (outer_p : path) : unit =
  let tmp_indices = List.init (List.length indices) (fun _ -> fresh_var ()) in
  set_indices_internal tmp_indices outer_p;
  set_indices_internal indices outer_p;