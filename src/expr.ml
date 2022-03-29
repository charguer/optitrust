open Target
include Expr_basic



(* [replace_fun code tg] expects the target to point to a function call,
    it then replaces the name of the function call with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which has the same
      signature as function whose call is targeted by [tg]
*)
let replace_fun ?(inline : bool = false) (name : string) : Target.Transfo.t =
  iteri_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun i t (path_to_seq, local_path, i1) -> 
        let path_to_call = path_to_seq @ [Path.Dir_seq_nth i1] @ local_path in 
        let tg_to_call = target_of_path path_to_call in
        Expr_basic.replace_fun name tg_to_call;
        if inline then Function.inline tg_to_call;
    )
  



let replace_fun1 (name : string) (tg : target) : unit =
  
  Target.apply_on_targets (Expr_core.replace_fun name) tg
