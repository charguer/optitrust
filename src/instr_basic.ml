open Ast
open Target

(* [replace code tg] expects the target to point at an instruction,
    then it will replace this instruction with the code entered by the user, which is merged into
    the ast by doing a reparse of the full ast.
*)
let replace (code : string) : Target.Transfo.t =
  Target.reparse_after(Target.apply_on_targets (Instr_core.replace code))

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

let move ~target:(where : Target.target) (tg : Target.target) : unit =
  Trace.call (fun t -> 
    let tg_where_path_seq,where_index = Target.resolve_target_between_exactly_one where t in
    let tg_path = Target.resolve_target_exactly_one tg t in
    let tg_path_to_seq, tg_index = Internal.isolate_last_dir_in_seq tg_path in
    if tg_where_path_seq <> tg_path_to_seq then fail None "move: the relative target should be in the same block as the main target";
    Marks.add "tmp_mark_move" (Target.target_of_path tg_path);
    Target.apply_on_targets (Instr_core.move where_index tg_index ) (Target.target_of_path tg_path_to_seq);
    delete [Target.cMark "tmp_mark_move"];
  ) 

(* [read_last_write ~write tg] expects the target [tg] to point to a read operation, then it
    replaces the trm corresponding to that read operation with the one at [write].
 *)
let read_last_write ~write:(write : Target.target) (tg : Target.target) : unit =
  let write_trm = Target.get_trm_at (write @ [dRHS]) in
  Target.apply_on_targets (fun t p -> Target.apply_on_path (fun _ -> write_trm) t p) tg



