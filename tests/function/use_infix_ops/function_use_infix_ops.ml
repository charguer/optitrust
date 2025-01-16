open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

  !! Function.use_infix_ops ~allow_identity:false [nbMulti; cWriteVar "a"];
  !! Function.use_infix_ops ~indepth:true [cFunBody "g"];
)

(** TODO
  for (int i = 0; i < 2; i += 1) {
    x += 1;
  }
  (*
  let use_prepostfix_ops_on (t : trm) : trm =
    match t with

  trm_has_cstyle Postfix_step range.step
  *)
*)
