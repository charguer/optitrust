open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* Functions *)
  !! Variable_basic.inline [cTopFunDef "f"];
  !! Function_basic.beta [cTopFunDef "test_fun"; cCall ""];

  (* Class methods *)
  !! Variable_basic.inline [cFunDef "f_X"];
  !! Function_basic.beta [cTopFunDef "test_method"; cCall ""];

  (* Namespaces *)
  (* FIXME:
    !! Variable_basic.unfold ~accept_functions:true [cFunDef "h"];
    !! Function_basic.beta [cTopFunDef "g"; cCall ""]; *)
)
