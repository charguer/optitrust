open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cLabelBody "mark"];
  (* TODO: ~acc_in_place:false should be the default
     TODO: if ~acc is provided, then ~acc_in_place must be false, else error
  *)

  (* TODO: perhaps this demo needs to be moved in the combi unit test? *)
  !! Matrix_basic.reorder_dims ~order:[3;2;1;0] () [cLabelBody "mark"; cFun ~regexp:true "M.\\(ALLOC\\|INDEX\\)"];
  (* TODO: remove the useless unit above *) (* TODO: the target above does not reach enough many targets *)
)