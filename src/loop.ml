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
        let t = Generic_core.var_init_detach i t (p @ [Dir_body]) in
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
  Target.apply_on_transformed_targets (Internal.get_trm_in_surrounding_loop)
    (fun (p, i) t ->
       match upto with 
       | "" -> Loop_core.invariant i t p
       | _ -> 
              let nb = ref 0 in
              let quit_loop = ref false in
              let tmp_p = ref [] in
              Tools.printf "%d\n" !nb;
              tmp_p := List.rev(List.tl (List.rev p));
              while not !quit_loop do
                let (tg_trm, _) = Path.resolve_path !tmp_p t in
                Tools.printf "%d\n" !nb;
                match  tg_trm.desc with 
                | Trm_for _ -> 
                  let index = for_loop_index tg_trm in
                  if index = upto then quit_loop := true
                    else 
                      nb := !nb + 1;
                      Tools.printf "%d\n" !nb;
                      tmp_p := List.rev(List.tl (List.rev !tmp_p))
                | _ -> 
                  Tools.printf "%d\n" !nb;
                  nb := !nb + 1;
                  tmp_p := List.rev(List.tl (List.rev !tmp_p))
              done; 
              let nb_list = Tools.range 1 !nb in
              List.fold_left (fun t _ind -> Loop_core.invariant i t p ) t nb_list              
    ) tg;
  Internal.nobrace_remove_and_exit ()