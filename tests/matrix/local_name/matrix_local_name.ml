open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  let a = find_var_in_current_ast "a" in
  !! Matrix_basic.local_name a ~into:"x" [cFunBody "main"; cFor "i"];

  (*
  let b = find_var_in_current_ast "b" in
  !! Matrix_basic.local_name b ~into:"y" ~alloc_instr:[cWriteVar "b"] [cFunBody "main"; cFor "j"]; *)

  let x = find_var_in_current_ast "c" in
  let n = find_var_in_current_ast "n" in
  !! Matrix_basic.local_name x ~into:"z" ~type_and_dims:(typ_int, [trm_var n]) [cFunBody "f"; dSeqNth 0];
)
