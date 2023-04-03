open Optitrust
open Target
(* open Ast *)

let _ = Flags.pretty_matrix_notation := true

let (~~) f a b = f b a

let _ = Run.script_cpp (fun () ->
  (* FIXME: duplicates even with suffix *)
  !! ~~List.iter ["conv3x3"; "sobelX"; "sobelY"; "binomial"; "mul"; "coarsity"] (fun fun_to_inline ->
    Function.inline ~delete:true ~vars:(Variable.Rename.add_suffix ("_" ^ fun_to_inline)) [nbMulti; cFun fun_to_inline];
  );
  (* TODO:
  !! ~~List.iter ["sobelX"; "sobelY"; "binomial"] (fun weights_to_inline ->
    Variable.inline [nbMulti; cVarDef ("weights_" ^ weights_to_inline)];
  );
  - simplify literal array accesses
     *)
  (* FIXME: reparse required to remove blank lines,
     otherwise fusion fails *)
  (* TODO: ~nb_loops + better API *)
  (* TODO:
  !!! Loop.fusion_targets ~nb_loops:2 [cFor ~body:[cOr [[cArrayWrite "ix"]; [cArrayWrite ["iy"]]] "y"];
  *)
  !!! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "ixx"] "y"];
  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "ixx"] "x"];
  (* TODO: fuse sxx/sxy/syy/coarsity *)
  !! Omp.header ();
  show [cFunDef "conv3x3"];
)