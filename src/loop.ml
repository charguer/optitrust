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
let hoist (x_step : var) (tg : Target.target) : unit =
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
          Loop_basic.hoist x_step (Target.target_of_path p);
        | false -> Loop_basic.hoist x_step (Target.target_of_path p)
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
(* TODO: discuss this LATER: loop_to_move should be a target; *)
let move ?(before : string = "") ?(after : string = "") (loop_to_move : string) : unit =
  Trace.call (fun t ->
    let move_where, target_loop = match before, after with
  | "", _ -> "after", [Target.cFor loop_to_move]
  | _, "" -> "before", [Target.cFor before]
  | _ -> fail None "move: make sure you specify where to move the loop, don't give both before and after directives" in
  let exp = Constr.resolve_target_exactly_one target_loop t in
  let (loop, _) = Path.resolve_path exp t in
  let indices_list = Internal.get_loop_nest_indices loop in
  match move_where with
  | "after" ->
    let indices_list = Tools.chop_list_after after indices_list in
    let counter = ref (List.length indices_list) in
    while (!counter <> 0) do
      counter := !counter - 1;
      Loop_basic.interchange [Target.cFor loop_to_move];
    done
  | "before" ->
    let indices_list = Tools.chop_list_after loop_to_move indices_list in
    List.iter (fun x -> Loop_basic.interchange [Target.cFor x]) (List.rev indices_list)
  | _ -> fail t.loc "move: something went wrong"
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



(* An automated version of coloring and reordering *)
let pic_coloring (tile_size : int) (color_size : int) (ds : string list) (tg : Target.target) : unit =
  let _splitted_ds = Tools.extract 0 (List.length ds - 2) ds in
  let bs = List.map (fun s -> "b" ^ s) ds in
  let splitted_bs = Tools.extract 0 (List.length bs - 2) bs in
  let _last_bs = match fst splitted_bs with
  | [x] -> x
  | _ -> failwith "coloring:expected the last element of bs" in
  let cs = List.map (fun s -> "c" ^ s) ds in
  let splitted_cs = Tools.extract 0 (List.length cs - 2) cs in
  let _last_cs = match fst splitted_cs with
  | [x] -> x
  | _ -> failwith "coloring:expected the last element of cs" in
  let tile = string_of_int tile_size in
  let color = string_of_int color_size in
  List.iter2 (fun d b -> Loop_basic.tile tile ~index:b (tg @ [Target.cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color color ~index:c (tg @ [Target.cFor b])) bs cs
  (* List.iter (fun b -> move b ~after:last_cs) (snd splitted_bs); *)
  (* List.iter (fun d -> move d ~after:last_bs) (snd _splitted_ds) *)


(* let loop_reorder (indices : var list) (tg : Target.target) : unit =
  Target.apply_on_targets () *)

(* TODO:
   Loop.reorder list_of_indices tg_first_loop
   let list_of_indces = (add_prefix "c" dims) @ (add_prefix "b" dims) @ dims
*)

let reorder (ordered_indices : var list) (tg : Target.target) : unit =
    Target.iter_on_targets (fun t p ->
      let tg_loop, _ = Path.resolve_path p t in
      let current_indices = Internal.get_loop_nest_indices tg_loop in
      if (List.length current_indices <> List.length ordered_indices) 
        then fail tg_loop.loc "reorder: reordering does not change the number of nested loops"
        else 
          begin
          let index_map : int varmap ref  = ref String_map.empty in
          List.iteri (fun i x -> index_map := String_map.add x i !index_map) ordered_indices;
          let _sorted_indices = Tools.bubble_sort (
            fun x y ->
            let targeted_ind_x = begin match (String_map.find_opt x !index_map) with
            | Some i -> i
            | None -> fail tg_loop.loc "reorder: the ordered list you entered contains indices of loops which don't belong the targeted scope"
            end in
            let targeted_ind_y = begin match (String_map.find_opt y !index_map) with
            | Some i -> i
            | None -> fail tg_loop.loc "reorder: the ordered list you entered contains indices of loops which don't belong the targeted scope"
            end in
            if targeted_ind_x > targeted_ind_y then 
              begin
              Loop_basic.interchange [Target.cFor x];
              true
              end
              else false

          ) current_indices in ()
          end
    ) tg

let pic_coloring1 (tile_size: int) (color_size : int) (ds : string list) (tg : Target.target) : unit =
  let add_prefix (prefix : string) (indices : var list) : var list =
    List.map (fun x -> x ^ prefix) indices  
    in
  let bs = add_prefix "b" ds in
  let cs = add_prefix "c" ds in
  let list_of_indices = bs @ cs @ ds in
  let tile =  string_of_int tile_size in
  let color = string_of_int color_size in
  List.iter2 (fun d b -> Loop_basic.tile tile ~index:b (tg @ [Target.cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color color ~index:c (tg @ [Target.cFor b])) bs cs;
  reorder list_of_indices [Target.cFor (List.nth cs 0)]
