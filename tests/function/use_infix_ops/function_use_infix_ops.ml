open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->

    !! Function.use_infix_ops ~indepth:true [cFunDef "g"; dBody];
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
