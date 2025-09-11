open Optitrust
open Prelude
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->

  let (a, _) = find_var "a" [] in
  !! Matrix_basic.local_name a ~into:"x" [cFunBody "main"; cFor "i"];

  (*
  let b = find_var "b" in
  !! Matrix_basic.local_name b ~into:"y" ~alloc_instr:[cWriteVar "b"] [cFunBody "main"; cFor "j"]; *)

  let (x, _) = find_var "c" [] in
  let (n, _) = find_var "n" [] in
  !! Matrix_basic.local_name x ~into:"z" ~type_and_dims:(typ_int, [trm_var n]) [cFunBody "f"; dSeqNth 0];
)
