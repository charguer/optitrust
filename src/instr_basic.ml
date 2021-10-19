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

(* [move ~before ~after tg] expects the target [tg] to point to an instruction
      inside a sequence, then it will move this instruction before the target [before]
      or after the target [after].

      Note: Only one of [before] or [after] should be specified
*)
let move ?(before : target = []) ?(after : target = []) : Target.Transfo.t  =
  let rel_tg =
  begin match before, after with
  | [], [] -> fail None "move: the relative target should be sepcified"
  | _,[] -> before
  | [], _ -> after
  | _ -> fail None "move: only before or after can be given as argumentsn but not both"
  end in
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t ->
      let ps = Target.resolve_target_exactly_one rel_tg t in
      let (p_instr,i_instr) = Internal.isolate_last_dir_in_seq ps in
      if p_instr <> p then fail t.loc "move: before or after should resolve to an instruction relative to the main target";
      Instr_core.move i i_instr t p)


(* [delete tg] expects the target [tg] to point to an instruction inside a sequence
      then it will replace that instruction with a unit literal. To remove this trm from the
      ast a final reparse is done.
*)
let delete : Target.Transfo.t =
  Target.reparse_after (
    Target.apply_on_targets (Target.apply_on_path (fun _ -> trm_unit ())) (* TODO: trm_unit should be trm_seq nobrace; and no reparse but instead remove_nobrace *)
    )

(* [read_last_write ~write tg] expects the target [tg] to point to a read operation, then it
    replaces the trm corresponding to that read operation with the one at [write].
 *)
let read_last_write ~write:(write : Target.target) (tg : Target.target) : unit =
  let write_trm = Target.get_trm_at (write @ [dRHS]) in
  Target.apply_on_targets (fun t p -> Target.apply_on_path (fun _ -> write_trm) t p) tg



