open Ast
open Target

(* [replace code tg] expects the target to point at an instruction,
    then it will replace this instruction with [node]. Note that [node] can be
    also code entered as string which is transformed into a trm through function code
    then this node is merged into the ast by doing a reparse of the full ast.
*)
let replace (node : trm) : Target.Transfo.t =
  let reparse = not (is_trm node) in
  Target.reparse_after ~reparse (Target.apply_on_targets (Instr_core.replace node))

(* [replace_fun code tg] expects the target to point to a function call,
    it then replaces the name of the function call with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which has the same
      signature as function whose call is targeted by [tg]
*)
let replace_fun (name : string) (tg : target) : unit =
  Target.apply_on_targets (Instr_core.replace_fun name) tg


(* [delete tg] expects the target [tg] to point to an instruction inside a sequence
      then it will remove that instruciton from that sequence
*)
let delete : Target.Transfo.t =
  Sequence_basic.delete 

(* [move ~target tg] expects the target [tg] to point to the instruction which is 
    going to be moved at the relative target [where]
*)
let move ~dest:(where : Target.target) (tg : Target.target) : unit = 
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> 
      let tg_dest_path_seq, dest_index = Target.resolve_target_between_exactly_one where t in
      if tg_dest_path_seq <> p then fail None "move: the destination target should be unique and belong to the same block as the main targets";
      Instr_core.move dest_index i t p
    ) tg

(* [read_last_write ~write tg] expects the target [tg] to point to a read operation, then it
    replaces the trm corresponding to that read operation with the one at [write].
 *)
let read_last_write ~write:(write : Target.target) (tg : Target.target) : unit =
  let write_trm = Target.get_trm_at (write @ [dRHS]) in
  Target.apply_on_targets (fun t p -> Target.apply_on_path (fun _ -> write_trm) t p) tg


(* [accumulate tg] expects the target [tg] to point to a block of write operations in the same memory location 
    then it will do an acumulation of those trms .
    Ex.
    int x;
    {
      x += 1;
      x += 2;
      x += 3;
    }
    is transformed to x += 1+2+3
*)
let accumulate : Target.Transfo.t =
  Target.apply_on_targets (Instr_core.accumulate)

