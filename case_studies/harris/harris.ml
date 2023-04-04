open Optitrust
open Target
(* open Ast *)

let _ = Flags.pretty_matrix_notation := true
(* let _ = Flags.analyse_stats := true *)

module Matrix = struct
  include Matrix

  let elim_accesses (name : string) : unit =
    (* FIXME:
    show [cArrayRead "weights_sobelX"];
    show [sExpr "weights_sobelX["];
    *)
    Matrix.elim_mops [nbMulti; sExpr (name ^ "[")];
    Arrays.elim_accesses [nbMulti; cVarDef name]
end

let _ = Run.script_cpp (fun () ->
  (*
(* FIXME: duplicates even with suffix *)
!! ["conv3x3"; "sobelX"; "sobelY"; (* "binomial"; *) "mul"; "coarsity"] |>  List.iter (fun fun_to_inline ->
  Function.inline ~delete:true ~vars:(Variable.Rename.add_suffix ("_" ^ fun_to_inline)) [nbMulti; cFun fun_to_inline];
);
*)
  bigstep "inline operators";
  !! Function.inline ~delete:true [nbMulti; cFun "conv3x3"];
  !! Matrix.elim_accesses "weights";
  !! ["sobelX"; "sobelY"; "binomial"; "mul"; "coarsity"] |> List.iter (fun fun_to_inline ->
    Function.inline ~delete:true [nbMulti; cFun fun_to_inline];
  );
  !! ["h1"; "w1"; "h2"; "w2"] |> List.iter (fun var_to_inline ->
    Variable.inline [cVarDef var_to_inline]
  );

  bigstep "fuse opeators";
  (* FIXME: reparse required to remove blank lines,
     otherwise fusion fails *)
  (* TODO: ~nb_loops + better API *)
  (* TODO:
  !!! Loop.fusion_targets ~nb_loops:2 [cFor ~body:[cOr [[cArrayWrite "ix"]; [cArrayWrite ["iy"]]] "y"];
  *)
  !!! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "ix"] "y"];
  !! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "ix"] "x"];
  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "ixx"] "y"];
  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "ixx"] "x"];
  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "sxx"] "y"];
  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "sxx"] "x"];
  (* TODO: fuse ixx/sxx/coarsity as well, requires recomputing *)

  bigstep "circular buffers";

  bigstep "parallelism";
  !! Omp.header ();

  bigstep "code details";
)