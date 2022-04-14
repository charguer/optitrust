open Ast
open Target

(* [delete tg] expects the target [tg] to point at an instruction inside a sequence
      then it will remove that instruciton from that sequence *)
let delete : Target.Transfo.t =
  Sequence_basic.delete

(* [move ~target tg] expects the target [tg] to point at the instruction which is
    going to be copied to the relative target [where], if the original should be deleted
    then ~delete:true should be passed as argument *)
let copy ?(rev : bool = false) ?(delete : bool = false) ?(dest:Target.target = []) (tg : Target.target) : unit =
  Target.apply_on_transformed_targets ~rev (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) ->
      let tg_dest_path_seq, dest_index = if dest = [] then p, i+1 else Target.resolve_target_between_exactly_one dest t in
      if tg_dest_path_seq <> p then fail None "move: the destination target should be unique and belong to the same block as the main targets";
      Instr_core.copy dest_index i delete t p
    ) tg


(* [move ~target tg] expects the target [tg] to point at the instruction which is
    going to be moved at the relative target [where]

   @correctness: Correct if the swapped instructions are parallelizable:
   {H1 * H} instr1 {H1' * H} and {H2 * H} instr2 {H2' * H}
   which lead globally to the derivation
   {H1 * H2 * H} instr1 {H1' * H2 * H} instr2 {H1' * H2' * H}
   we can build the same postcondition with
   {H1 * H2 * H} instr2 {H1 * H2' * H} instr1 {H1' * H2' * H}
   
   This is sufficient but not necessary, a manual commutation proof can be used
   as well. *)
let move ?(rev : bool = false) ~dest:(where : Target.target) (tg : Target.target) : unit = 
  copy ~rev ~delete:true ~dest:where tg

(* [read_last_write ~write tg] expects the target [tg] to point at a read operation, then it
    replaces the trm corresponding to that read operation with the one at [write]. 
    If the given target doesn't point to a get operation then the transformation will move the 
    target to the first ancestor node it find that is a get operation.

   @correctness: the read expression must be pure, and its evaluation must not
   have changed since the write.*)

let read_last_write ~write:(write : Target.target) (tg : Target.target) : unit = 
  let write_trm = match Target.get_trm_at write with 
  | Some wt -> wt
  | None -> fail None "uninline: write target does point to any node" in 
  let written_trm = 
    match write_trm.desc with 
    | Trm_apps (_, [_;rhs]) when is_set_operation write_trm -> rhs
    | Trm_let (_, _, init) -> 
      begin match get_init_val init with 
      | Some init -> init
      | None -> fail write_trm.loc "read_last_write: the targeted write operation should be either a set operation or
           or an initialized variable declaration"
      end
    | _ -> fail write_trm.loc "read_last_write: the targeted write operation should be either a set operation or 
      an initialized variable declaration" in 
  Target.apply_on_targets (fun t p -> 
    let get_op_path = Internal.get_surrounding_trm (fun t -> (is_get_operation t)) p t in 
    if get_op_path = [] then t else 
    Target.apply_on_path (fun _ -> written_trm) t p) tg

(* [accumulate tg] expects the target [tg] to point at a block of write operations in the same memory location
    then it will do an acumulation of those trms .
    Ex.
    int x;
    {
      x += 1;
      x += 2;
      x += 3;
    }
    is transformed to x += 1+2+3 *)
let accumulate : Target.Transfo.t =
  Target.apply_on_targets (Instr_core.accumulate)


(* LATER: move this to Expr_basic.ml *)
(* [view_subterms tg] displays on stdout all the subterms of the targeted term.
   For viewing on stdout all subterms of a program, use:
     Instr.view_subterms [];
   which is like
     Instr.view_subterms [dRoot];
   and possibly specify a regexp to investigate:
     Instr.view_subterms ~constr:(sInstr "+= 2") [dRoot];
   Note that this is for debugging purpose only. *)
let view_subterms ?(constr:Constr.constr option) ?(rexp : Constr.rexp option) (tg : Target.target) : unit =
  let tg = if tg = [] then [dRoot] else tg in
  let ro = match constr, rexp with
    | None, None -> None
    | Some (Constr_regexp r), None -> Some r
    | Some _, None -> fail None "view_subterms: [~constr] argument must be a regexp-constraint (e.g., sInstr or sExpr)"
    | None, Some r -> Some r
    | Some _, Some _ -> fail None "view_subterms: cannot provide both [~constr] and [rexp]"
    in
  let stringreprs = Target.compute_stringreprs_and_update_ast (fun _ -> true) in
  (* for debug: AstC_to_c.print_stringreprs stringreprs; *)
  Target.apply_on_targets (Instr_core.view_subterms stringreprs ro) tg

