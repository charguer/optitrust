open Ast
open Target

(* [replace code tg] expects the target to point at an instruction,
    then it will replace this instruction with the code entered by the user, which is merged into
    the ast by doing a reparse of the full ast.
*)
let replace (code : string) (tg : target) : unit =
  Target.apply_on_targets (Instr_core.replace code) tg;
  Trace.reparse()

(* [replace_fun code tg] expects the target to point to a function call,
    it then replaces the name of the function call with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which has the same
      signature as function whose call is targeted by [tg]
*)
let replace_fun (name : string) (tg : target) : unit =
  Target.apply_on_targets (Instr_core.replace_fun name) tg
  

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
