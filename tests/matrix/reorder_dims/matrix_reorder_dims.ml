open Optitrust
open Target
open Trm
let _ =
  Flags.check_validity := true;
  Flags.detailed_resources_in_trace := true;
  Flags.save_ast_for_steps := Some Steps_all

let _ = Run.script_cpp (fun _ ->

  !! Matrix.reorder_dims ~order:[2;1;0] [cVarDef "p"];
  !! Matrix.reorder_dims ~rotate_n:2 [cVarDef "q"];

)


(* trouve comment je peux faire des boucles avec les ghosts et après j'insère le consumes dans l'odre inversé et le produces dans l'ordre non inversé avec rien déjà  à l'interieue
 *)
(*  must be able to do a contract, avec un  *)

