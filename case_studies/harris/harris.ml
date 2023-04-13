open Optitrust
open Target
open Ast

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
  (* TODO: Function.specialize ??? *)
  (*
  !! Specialize.function_arg "conv3x3" [true; true; true; true; false; false; true] [nbMulti; cFun "conv2D"];
  *)
  !! Function.inline ~delete:true [nbMulti; cFun "conv2D"];
  (* TODO: ~nb_loops:2 *)
  !! Loop.unroll [nbMulti; cFor ~body:[cPlusEqVar "acc"] "j"];
  !! Loop.unroll [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  (* TODO?
  !! Instr.accumulate ~nb:9 [nbMulti; cVarDef "acc"]; *)
  (* remove 0.0f * x *)
  !! Matrix.elim_accesses "weights";
  !! ["grayscale"; "sobelX"; "sobelY"; "binomial"; "mul"; "coarsity"] |> List.iter (fun fun_to_inline ->
    Function.inline ~delete:true [nbMulti; cFun fun_to_inline];
  );
  !! ["h1"; "w1"; "h2"; "w2"] |> List.iter (fun var_to_inline ->
    Variable.inline [cVarDef var_to_inline]
  );
  !! Arith.(simpl gather) [nbMulti; cFor ""; dForStop];

  bigstep "fuse operators";
  (* FIXME: reparse required to remove blank lines,
     otherwise fusion fails *)
  (* TODO: ~nb_loops + better API *)
  (* TODO:
  !!! Loop.fusion_targets ~nb_loops:2 [cFor ~body:[cOr [[cArrayWrite "ix"]; [cArrayWrite ["iy"]]] "y"];
  TODO: check that loops extents are the same; or adjust them
  *)
  !! Sequence.intro_on_instr [cFor ~body:[cArrayWrite "ix"] "x"; dBody];
  !! Sequence.intro_on_instr [cFor ~body:[cArrayWrite "iy"] "x"; dBody];
  !!! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "ix"] "y"];
  !! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "ix"] "x"];

  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "ixx"] "y"];
  !! Loop.fusion ~nb:3 [cFor ~body:[cArrayWrite "ixx"] "x"];

  !! Sequence.intro_on_instr [cFor ~body:[cArrayWrite "sxx"] "x"; dBody];
  !! Sequence.intro_on_instr [cFor ~body:[cArrayWrite "sxy"] "x"; dBody];
  !! Sequence.intro_on_instr [cFor ~body:[cArrayWrite "syy"] "x"; dBody];
  !! Loop.fusion ~nb:4 [cFor ~body:[cArrayWrite "sxx"] "y"];
  !! Loop.fusion ~nb:4 [cFor ~body:[cArrayWrite "sxx"] "x"];

  !! Loop.shift (trm_int 2) [cFor ~body:[cArrayWrite "ix"] "y"];
  !! Loop.extend_range ~lower:ShiftToZero [cFor ~body:[cArrayWrite "ix"] "y"];
  !! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "gray"] "y"];

  (* TODO: inline ixx/... into sxx/... computation instead, requires recomputing, and Variable.bind with CSE. *)
  !! Loop.shift (trm_int 2) [cFor ~body:[cArrayWrite "ixx"] "y"];
  !! Loop.extend_range ~lower:ShiftToZero [cFor ~body:[cArrayWrite "ixx"] "y"];
  !! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "ix"] "y"];

  !! Loop.shift (trm_int 4) [cFor ~body:[cArrayWrite "out"] "y"];
  !! Loop.extend_range ~lower:ShiftToZero [cFor ~body:[cArrayWrite "out"] "y"];
  !! Loop.fusion ~nb:2 [cFor ~body:[cArrayWrite "ix"] "y"];

  bigstep "circular buffers";
  !! ["gray"; "ix"; "iy"] |> List.iter (fun to_fold ->
    Matrix.storage_folding ~var:to_fold ~dim:0 ~n:(trm_int 3) [cFunBody "harris"]
  );

  bigstep "parallelism";
  !! Omp.header ();

  bigstep "code details";
)