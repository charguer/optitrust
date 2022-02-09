open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.to_const [cVarDef "x"];
  )
"
int main() {
  int x = 3;
}
"

(* let _ = Flags.dump_ast_details := true *)

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.to_const [cVarDef "x"];
  !! Variable_basic.to_const [cVarDef "y"];
  !! Variable_basic.to_const [cVarDef "z"];
)

(* Note: recall that currently const references are not supported,
   see Ast_fromto_AstC *)