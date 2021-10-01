open Ast
include Variable_core.Rename
include Loop_basic

type rename = Variable_core.Rename.t
(* [hoist x_step tg] - expects target to point inside the declaration of the variable
    [x_step] - denotes the variable going to be hoisted outside the loop.
    This transformation is similar to the basic one except that it supports also
      undetached declarations contrary to the basic one. This is done by first checking if
      the declaration is detached or not. If it is not detached then we call another
      transformation which does that for us. Otherwise just apply the basic hoist transformation.
*)
let hoist ?(name : var = "${var}_step") (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let (tg_trm, _) = Path.resolve_path p t in
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
          Loop_basic.hoist ~name (Target.target_of_path p);
        | false -> Loop_basic.hoist ~name (Target.target_of_path p)
  ) tg

(* [fusion nb tg] expects [tg] to point to a for loop followed by two or more
    loops with the same range, start step and bound but different body.
    Then it's going to merge bodies of all those loops into a single loop.
    [nb] - denotes the number of loops to consider.
*)
let fusion ?(nb : int = 2) (tg : Target.target) : unit =
  let label = "__TEMP_LABEL" in
  Sequence_basic.intro nb ~label tg;
  Loop_basic.fusion_on_block [Target.cLabel label]

(* LATER: documentation?generalize? *)
let invariant ?(upto : string = "") (tg : Target.target) : unit =
  Internal.nobrace_remove_after( fun _ ->
  Target.iter_on_targets (fun t exp ->
    let (p, _) = Internal.get_trm_in_surrounding_loop exp in
    match upto with
    | "" -> Loop_basic.invariant tg
    | _ ->
          let quit_loop = ref false in
          let tmp_p = ref [] in
          tmp_p := List.rev(List.tl (List.rev p));
          while not !quit_loop do
            let (tg_trm, _) = Path.resolve_path !tmp_p t in
            match  tg_trm.desc with
            | Trm_for _ ->
              let index = for_loop_index tg_trm in
              if index = upto then
                  begin
                  Loop_basic.invariant tg;
                  quit_loop := true;
                  end
                else
                  Loop_basic.invariant tg;
                  tmp_p := List.rev(List.tl (List.rev !tmp_p))
            | _ ->
              Loop_basic.invariant tg;
              tmp_p := List.rev(List.tl (List.rev !tmp_p))
            done
  ) tg
)

(* [move before after loop_to_move] move one loop before or after another loop in
    a "sequence"(not in the context of Optitrust) of nested loops.
    [before] - a default argument given as empty string, if the user wants to move
      [loop_to_move] before another loop then it should use this default argument with the
      value the the quoted loop intex
    [after] - similar to [after] but now is the index of the loop after whom
      we want to move [loop_to_move]
*)
let move ?(before : Target.target = []) ?(after : Target.target = []) (loop_to_move : Target.target) : unit =
  Trace.call (fun t ->
   let loop_to_move_path = Target.resolve_target_exactly_one loop_to_move t in
   let loop_to_move_trm, _ = Path.resolve_path loop_to_move_path t in
   let loop_to_move_nested_indices = Internal.get_loop_nest_indices loop_to_move_trm in
   let loop_to_move_index  = List.nth loop_to_move_nested_indices 0 in
   begin match before, after with
   | [], [] -> fail None  "move: the before target or after target are mandatory please enter only one of them"
   | [], _ ->
    let targeted_loop_path = Target.resolve_target_exactly_one after t in
    let targeted_loop, _ = Path.resolve_path targeted_loop_path t in
    let targeted_loop_nested_indices = Internal.get_loop_nest_indices targeted_loop in
    let targeted_loop_index = List.nth targeted_loop_nested_indices  0 in
    if List.mem targeted_loop_index loop_to_move_nested_indices
      then begin
           let choped_indices = Tools.chop_list_after targeted_loop_index loop_to_move_nested_indices in
           List.iter (fun _ -> Loop_basic.interchange loop_to_move) choped_indices
           end
      else if List.mem loop_to_move_index targeted_loop_nested_indices then
        begin
        let choped_indices = Tools.chop_list_after loop_to_move_index targeted_loop_nested_indices in
        let choped_indices = Tools.chop_list_after targeted_loop_index (List.rev choped_indices) in
        List.iter (fun x -> Loop_basic.interchange [Target.cFor x]) choped_indices
        end
      else fail loop_to_move_trm.loc "move: the given targets are not correct"

   | _ , [] ->
    let targeted_loop_path = Target.resolve_target_exactly_one before t in
    let targeted_loop, _ = Path.resolve_path targeted_loop_path t in
    let targeted_loop_nested_indices = Internal.get_loop_nest_indices targeted_loop in
    let targeted_loop_index = List.nth targeted_loop_nested_indices  0 in
    if List.mem targeted_loop_index loop_to_move_nested_indices
      then begin
           let choped_indices = Tools.chop_list_after targeted_loop_index loop_to_move_nested_indices in
           let choped_indices = Tools.chop_list_after loop_to_move_index (List.rev choped_indices) in
           List.iter (fun _ -> Loop_basic.interchange loop_to_move) (List.rev choped_indices)
           end
      else if List.mem loop_to_move_index targeted_loop_nested_indices then
        begin
        let choped_indices = Tools.chop_list_after loop_to_move_index targeted_loop_nested_indices in
        List.iter (fun x -> Loop_basic.interchange [Target.cFor x]) (List.rev choped_indices)
        end
      else fail loop_to_move_trm.loc "move: the given targets are not correct"

   | _  -> fail None "move: only one of target before or after should be given"
   end
  )

(* [unroll] expects the target to point to a loop. It the checks if teh loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the
    the instructions out of the loop and the loop will be removed.
    Assumption C should be a literal, this is needed to compute the number
    of sequences to generate.
    braces:true to keep the sequences
*)
let unroll ?(braces:bool=false) ?(blocks : int list = []) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let mylabel = "__TEMP_LABEL" in
    let (tg_loop_trm,_) = Path.resolve_path p t in
    match tg_loop_trm.desc with
    | Trm_for (_, _, _, stop, _, _) ->
      begin match stop.desc with
      | Trm_apps (_,[_;bnd]) ->
        begin match bnd.desc with
        | Trm_val (Val_lit (Lit_int n)) -> Loop_basic.unroll ~label:mylabel tg;
          let block_list = Tools.range 0 (n-1) in
          List.iter (fun x -> Variable_basic.rename (AddSuffix (string_of_int x)) ([Target.tIndex ~nb:n x; Target.cLabel mylabel; Target.dBody;Target.cSeq ()])) block_list;
          Sequence_basic.partition blocks [Target.nbExact n;Target.cLabel mylabel; Target.dBody;Target.cSeq ()]

        | Trm_var x -> Variable_basic.inline [Target.cVarDef x];
                       Internal.nobrace_remove_after (fun _-> Loop_basic.unroll ~label:mylabel tg);
          let var_decl = match Internal.toplevel_decl x t with
            | Some d -> d
            | None -> fail t.loc "unroll: could not find the declaration of the variable"
          in
          let lit_n = get_init_val var_decl in
          let n = match (get_lit_from_trm_lit lit_n)  with
          | Lit_int n -> n
          | _ -> fail t.loc "unroll: could not get the number of steps to unroll" in
          let block_list = Tools.range 0 (n-1) in
          List.iter (fun x ->
            Variable_basic.rename (AddSuffix (string_of_int x)) ([Target.tIndex ~nb:(n+1) x; Target.cLabel mylabel; Target.dBody;Target.cSeq ()])
          ) block_list;
          List.iter (fun x ->
             Sequence_basic.partition ~visible:braces blocks [Target.cLabel mylabel; Target.dBody; Target.dNth x]
          ) block_list;
          Sequence_basic.reorder_blocks [Target.cLabel mylabel; Target.dBody];
          Label_basic.remove [Target.cLabel mylabel]
        | _ -> fail bnd.loc "unroll: expected either a constant variable or a literal"
        end
      | _ -> fail t.loc "unroll: expected an addition between two trms"
      end
    | _ -> fail t.loc "unroll: expected a simple loop"
  ) tg

(* [reorder order]  expects the target [tg] to point to the first loop included in the sorting
    the it will reorder the nested loops based on [order]
    Assumption:
      Loops are nested by using sequences
*)
let reorder ?(order : var list = []) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let tg_loop, _ = Path.resolve_path p t in
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


(* TODO: tg should be a target on the outer loop, not on its context.
   I think using [tg @ [cForNestedAtDepth i]] would work for targeting the loop at depth i  *)
(* [pic_coloring tile_size color_size ds tg] expects the target [tg] to point to the first loop
      on which tiling(refer to Loop_basic.tile ) is going to be applied. Then on the loop comming right after the target [tg]
      coloring transformation (refere to Loop_basic.color).
      Finally a reorder is going to be applied by using reorder transformation (refer to reorder).
      Assumption:
        The target should be a nested loop 
*)
let pic_coloring (tile_size : int) (color_size : int) (ds : string list) (tg : Target.target) : unit =
  let bs = Tools.add_prefix "b" ds in
  let cs = Tools.add_prefix "c" ds in
  let first_cs = List.nth cs 0 in
  let order = cs @ bs @ ds in
  let tile = string_of_int tile_size in
  let color = string_of_int color_size in
  List.iter2 (fun d b -> Loop_basic.tile tile ~index:b (tg @ [Target.cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color color ~index:c (tg @ [Target.cFor b])) bs cs;
  reorder ~order [Target.cFor first_cs]
