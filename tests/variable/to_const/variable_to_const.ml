open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->


  !! Variable_basic.to_const [cVarDef "x"];
  !! Variable_basic.to_const [cVarDef "y"];
  !! Variable_basic.to_const [cVarDef "z"];

  !! Variable_basic.to_const [cVarDef "v"];
  !! Variable_basic.to_const [cVarDef "w"];

  !! Variable_basic.to_const [cVarDef "p"];
  (* LATER/ to const on array
    !! Variable_basic.to_const [cVarDef "q"];
   LATER to_const ?(simpl_deref:bool=true)
     => performs simpl_deref on the sequence that contains varDef q *)
  (*!! Variable_basic.simpl_deref ~indepth:true [cVarDef "r"];*)
  !! Variable_basic.to_const [cVarDef "r"];

  !! Variable_basic.to_const [cVarDef "f"];
  !! Variable_basic.to_const [cVarDef "g"];

  (* LATER: check failures

  !! Variable_basic.to_const [cVarDef "bad"];
  !! Trace.reparse();
  !! Trace.failure_expected (fun () -> ());
  *)

)

(* Note: recall that currently const references are not supported,
   see Ast_fromto_AstC *)
