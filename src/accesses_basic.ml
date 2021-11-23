open Ast


(* [transform f_get f_set]: expects the target [tg] to point to a set operation or a get operation
    if is a get operation then f_get will be applied on the node represented by target [tg]. Otherwise
    it is a set oepration then f_set will be applied on the second argument of the targeted node
*)
let transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets (Accesses_core.transform f_get  f_set) )

(* [scale ~factor ~factor_ast tg] this function is a specialization of the function transform where the functions f_get and f_set
    are given explicitly as the division and multiplication operations respectively
*)
let scale ?(factor : string option) ?(factor_ast : trm option) (tg : Target.target) : unit =
  begin try
  let arg = combine_strm factor factor_ast in
  let f_get t = Arith_core.apply_aux Binop_div arg t in
  let f_set t = Arith_core.apply_aux Binop_mul arg t in
  let reparse = not (is_trm arg) in
  transform ~reparse f_get f_set tg
  with | Ast_and_code_provided -> fail None "scale: please choose between factor and factor_ast arg"
       | No_ast_or_code_provided -> fail None "scale: expected for the code entered as string or the ast of that code"
  end

(* [shift ~factor ~factor_ast tg] this function is a specialization of the function transform where the functions f_get and f_set
    are given explicitly as the substraction  and addition respectively
*)
let shift (* TODO: ?neg:bool=false *) ?(factor : string option) ?(factor_ast : trm option ) (tg : Target.target) : unit =
  begin try
  let arg = combine_strm factor factor_ast in
   (* TODO: let op_get, op_set = if neg then (Binop_add, Binop_sub) else (Binop_sub, Binop_add) in *)
  let f_get t = Arith_core.apply_aux Binop_sub arg t in
  let f_set t = Arith_core.apply_aux Binop_add arg t in
  let reparse = not (is_trm arg) in
  transform ~reparse f_get f_set tg
  with | Ast_and_code_provided -> fail None "shift: please choose between factor and factor_ast arg"
       | No_ast_or_code_provided -> fail None "shift: expected the code entered as string or the ast of that code"
  end

 (* LATER: Define shift_access that applies to a target on accesses and calls shift on the parent path *)
