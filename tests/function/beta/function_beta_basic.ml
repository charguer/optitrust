open Optitrust
open Target

let _ = Flags.use_member_functions()

let _ = Run.script_cpp (fun _ ->
  (* Functions *)
  !! Variable_basic.inline [cTopFunDef "f"];
  !! Function_basic.beta [cTopFunDef "test_fun"; cFun ""];

  (* Class methods *)
  !! Variable_basic.inline [cFunDef "f_X"];
  !! Function_basic.beta [cTopFunDef "test_method"; cFun ""];

  (* Namespaces *)
  (* FIXME:
    !! Variable_basic.unfold ~accept_functions:true [cFunDef "h"];
    !! Function_basic.beta [cTopFunDef "g"; cFun ""]; *)
)
