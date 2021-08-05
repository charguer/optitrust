open Ast
(* [hoist x_step tg] - expects target to point inside the declaration of the variable
    [x_step] - denotes the variable going to be hoisted outside the loop.
    This transformation is similar to the basic one except that it supports also
      undetached declarations contrary to the basic one. This is done by first checking if
      the declaration is detached or not. If it is not detached then we call another
      transformation which does that for us. Otherwise just apply the basic hoist transformation.
*)
let hoist (x_step : var) (tg : Target.target) : unit =
  Internal.nobrace_enter ();
  Target.apply_on_transformed_targets(Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t -> 
      let (tg_trm,_) = Path.resolve_path (p @ [Dir_body;Dir_seq_nth i]) t in
      (* Check if detaching is needed *)
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
        let t = Variable_core.init_detach i t (p @ [Dir_body]) in
        let t = Loop_core.hoist x_step i t p in t
      | false -> let t = Loop_core.hoist x_step i t p in t) tg;
  Internal.nobrace_remove_and_exit ()


(* [fusion nb tg] expects [tg] to point to a for loop followed by two or more
    loops with the same range, start step and bound but different body.
    Then it's going to merge bodies of all those loops into a single loop.
    [nb] - denotes the number of loops to consider.
*)
let fusion ?(nb : int = 2) (tg : Target.target) : unit =
  let label = Tools.optitrust_label in
  Sequence_basic.intro nb ~label tg;
  Loop_basic.fusion_on_block [Target.cLabel label]


let invariant ?(upto : string = "") (tg : Target.target) : unit =
  Internal.nobrace_enter();
  let t = Trace.get_ast() in
  let exp =  Constr.resolve_target_exactly_one tg t in
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
            done;
  Internal.nobrace_remove_and_exit ()

(* [move before after loop_to_move] move one loop before or after another loop in
    a "sequence"(not in the context of Optitrust) of nested loops.
    [before] - a default argument given as empty string, if the user wants to move
      [loop_to_move] before another loop then it should use this default argument with the
      value the the quoted loop intex
    [after] - similar to [after] but now is the index of the loop after whom
      we want to move [loop_to_move]
*)
let move ?(before : string = "") ?(after : string = "") (loop_to_move : string) : unit =
  let t = Trace.get_ast() in
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


(* [unroll] expects the target to point to a loop. It the checks if teh loop
    is of the form for(int i = a; i < a + C; i++){..} then it will move the
    the instructions out of the loop and the loop will be removed.
    Assumption C should be a literal, this is needed to compute the number 
    of sequences to generate.
*)
let unroll (tg : Target.target) : unit =
  let t = Trace.get_ast () in
  let tg_loop_path =  Constr.resolve_target_exactly_one tg t in
  let (tg_loop_trm,_) = Path.resolve_path tg_loop_path t in
  match tg_loop_trm.desc with 
  | Trm_for (_, _, _, stop, _, _) ->
    begin match stop.desc with 
    | Trm_apps (_,[_;bnd]) ->
      begin match bnd.desc with 
      | Trm_val (Val_lit (Lit_int n)) -> Loop_basic.unroll ~label:"unroll" tg;
        let block_list = Tools.range 0 n in
        List.iter (fun x -> Variable_basic.rename (Postfix (string_of_int x)) ([Target.tIndex ~nb:n x ] @ tg @ [Target.cSeq ()])) block_list
      | Trm_var x -> Variable_basic.inline [Target.cVarDef x];
                     Loop_basic.unroll ~label:"unroll" tg;
        let var_decl = match Internal.toplevel_decl x t with 
          | Some d -> d
          | None -> fail t.loc "unroll: could not find the declaration of the variable"
        in
        let lit_n = get_initialization_trm var_decl in 
        let n = match (get_lit_from_trm_lit lit_n)  with
        | Lit_int n -> n
        | _ -> fail t.loc "unroll: could not get the size of unrollment" in
        let block_list = Tools.range 0 n in
        List.iter (fun x -> Variable_basic.rename (Postfix (string_of_int x)) ([Target.tIndex ~nb:n x ] @ tg @ [Target.cSeq ()])) block_list
      | _ -> fail bnd.loc "unroll: expected either a constant variable or a literal"
      end
    | _ -> fail t.loc "unroll: expected an addition between two trms"
    end
  | _ -> fail t.loc "unroll: expected a simple loop"

 