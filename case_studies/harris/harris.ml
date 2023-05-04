open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true
(* let _ = Flags.analyse_stats := true *)

module Function = struct
  include Function

  (* TODO:
    - cFuns = cOr ...
    Function.inline on a definition, finding all calls
    *)
  let inline_all (funs : vars) : unit =
    funs |> List.iter (fun f ->
      Function.inline ~delete:true [nbMulti; cFun f])
end

module Variable = struct
  include Variable

  (* TODO:
     - cVarDefs = cOr ...
     - cOrMap cVarDef vars ?
    *)
  let inline_all (vars : vars) : unit =
    vars |> List.iter (fun v -> Variable.inline [cVarDef v])
end

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

(* keep local *)
module Image = struct
  let fuse_loops_with_array_writes ?(nest_of = 1) ~(index : var) (arrays : var list) : unit =
    let writes = cOr (List.map (fun a -> [cArrayWrite a]) arrays) in
    Loop.fusion_targets ~nest_of [nbMulti; cFor ~body:[writes] index]

  let loop_align_stop_extend_start (index : var) ~(start : trm) ~(stop : trm) : unit =
    Loop.shift (StopAt stop) [nbMulti; cFor index];
    Loop.extend_range ~start:(ExtendTo start) [nbMulti; cFor index];
    (* TODO: transfo to remove useless if inside a for *)
end

let _ = Run.script_cpp (fun () ->
  bigstep "inline operators";
  !! Function.inline_all ["conv2D"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_accesses "weights";
  !! Function.inline_all ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "mul"; "coarsity"];
  (* cFunBody "harris"; cConstDef ""; *)
  !! Variable.inline_all ["h1"; "w1"; "h2"; "w2"];
  !! Arith.(simpl gather) [nbMulti; cFor ""; dForStop];

  bigstep "fuse operators";
  let rename_acc_of array = Variable.rename ~into:("acc_" ^ array) [cFor ~body:[cArrayWrite array] ""; cVarDef "acc"] in
  !! List.iter rename_acc_of ["ix"; "iy"; "sxx"; "sxy"; "syy"];
  let fuse ?(index = "y") = Image.fuse_loops_with_array_writes ~index in
  !! fuse ~nest_of:2 ["ix"; "iy"];
  !! fuse ~nest_of:2 ["ixx"; "ixy"; "iyy"];
  !! fuse ~nest_of:2 ["sxx"; "sxy"; "syy"; "out"];
  let elim_matrix m =
    Matrix.read_last_write ~write:[cArrayWrite m] [nbMulti; cArrayRead m];
    (* FIXME: dangerous transformation? *)
    (* TODO: Matrix.delete_not_read *)
    Instr.delete [cArrayWrite m];
    Matrix.delete ~var:m [cFunBody "harris"];
  in
  !! List.iter elim_matrix ["ixx"; "ixy"; "iyy"; "sxx"; "sxy"; "syy"];
  (* FIXME: dangerous, make transformation to remove empty loops *)
  !! Instr.delete [occIndex ~nb:4 2; cFor "y"];

  bigstep "overlapped tiling over lines";
  (* following could be Loop.tile: *)
  !!! Loop.slide ~size:(trm_int 32) ~step:(trm_int 32) [cFor ~body:[cArrayWrite "out"] "y"];
  !! Loop.slide ~size:(trm_int 34) ~step:(trm_int 32) [cFor ~body:[cArrayWrite "ix"] "y"];
  !! Loop.slide ~size:(trm_int 36) ~step:(trm_int 32) [cFor ~body:[cArrayWrite "gray"] "y"];
  !!! fuse ~index:"by" ["gray"; "ix"; "out"];

  bigstep "circular buffers";
  (* Without tiling:
  !! Image.loop_align_stop_extend_start "y" ~start:(trm_int 0) ~stop:(trm_var "h");
  *)
  !! Image.loop_align_stop_extend_start "y" ~start:(trm_var "by") ~stop:(expr "min(h + -4, by + 32)");
  !! fuse ["gray"; "ix"; "ixx"; "out"];
  (* TODO: introduce local buffers inside tile loops *)
  let circular_buffer v = Matrix.storage_folding ~dim:0 ~size:(trm_int 3) ~var:v [cFunBody "harris"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];

  bigstep "code details";
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  (* !! Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_ix"] ~fresh_name:"g${occ}" [cArrayRead "gray"]; *)
  !! Matrix.elim_mops [];

  bigstep "parallelism";
  !! Omp.header ();
  (* Intel compiler may be better at this:
    !! Omp.simd [nbMulti; cFor "x"]; *)
  (* !! Omp.parallel_for [cFor "by"]; *)
)