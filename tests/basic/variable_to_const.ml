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

  !! Variable_basic.to_const [cVarDef "v"];
  !! Variable_basic.to_const [cVarDef "w"];

  !! Variable_basic.to_const [cVarDef "p"];
  !! Variable_basic.to_const [cVarDef "q"];
  !! Variable_basic.to_const [cVarDef "r"];
)

(* Note: recall that currently const references are not supported,
   see Ast_fromto_AstC *)



   (*
   TODO try

   toconst x

exception Variable_to_const_abort

let aux u =
  match u with
  | trm_var y when y = x -> u
  | struct_access(u1, f) -> struct_get(aux u1, f)
  | _ -> raise Aux_abort

  (* LATER: check we don't need any support for arrays
   | array_access(u1, u2) -> struct_get(aux u1, u2) *)

let tr t =
  match t with
  | get(u) ->
      try (aux u)
      with Variable_to_const_abort -> t
  | _ -> t

*)