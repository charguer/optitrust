open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.to_const [cVarDef "x"];
  !! Variable_basic.to_const [cVarDef "y"];
  !! Variable_basic.to_const [cVarDef "z"];

  !! Variable_basic.to_const [cVarDef "v"];
  !! Variable_basic.to_const [cVarDef "w"];

  !! Variable_basic.to_const [cVarDef "p"];
  !! Variable_basic.to_const [cVarDef "q"];
  !! Variable_basic.to_const [cVarDef "r"];
  (* LATER/ to const on array
    !! Variable_basic.to_const [cVarDef "q"];
   LATER to_const ?(simpl_deref:bool=true)
     => performs simpl_deref on the sequence that contains varDef q *)
  (*!! Variable_basic.simpl_deref ~indepth:true [cVarDef "r"];*)
  !! Variable_basic.to_const [cVarDef "r"];



)

(* Note: recall that currently const references are not supported,
   see Ast_fromto_AstC *)
