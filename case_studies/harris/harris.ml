open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true
(* let _ = Flags.analyse_stats := true *)

module Matrix = struct
  include Matrix

  (* TODO: should this be in matrix.ml? *)
  let elim_constant (name : string) : unit =
    Matrix.elim_mops [nbMulti; cArrayRead name];
    Arrays.elim_constant [nbMulti; cVarDef name]
end

(* keep local *)
module Image = struct
  let fuse_loops_with_array_writes ?(nest_of = 1) ~(index : var) (arrays : var list) : unit =
    let writes = cOrMap cArrayWrite arrays in
    Loop.fusion_targets ~nest_of [nbMulti; cFor ~body:[writes] index]

  let loop_align_stop_extend_start (index : var) ~(start : trm) ~(stop : trm) : unit =
    Loop.shift (StopAt stop) [nbMulti; cFor index];
    Loop.extend_range ~start:(ExtendTo start) [nbMulti; cFor index];
    (* TODO: transfo to remove useless if inside a for *)
end

let _ = Run.script_cpp (fun () ->
  let simpl = fun tg -> Arith.(simpl_surrounding_expr gather (nbAny :: tg)) in
  let fuse ?(index = "y") = Image.fuse_loops_with_array_writes ~index in

  bigstep "inline operators";
  (* Function.inline_def *)
  !! Function.inline ~delete:true [nbMulti; cFun "conv2D"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_constant "weights";
  !! Function.inline ~delete:true [multi cFun ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "mul"; "coarsity"]];
  !! Variable.inline ~simpl [multi cVarDef ["h1"; "w1"; "h2"; "w2"]];

  bigstep "fuse operators";
  let rename_acc_of array = Variable.rename ~into:("acc_" ^ array) [cFor ~body:[cArrayWrite array] ""; cVarDef "acc"] in
  !! List.iter rename_acc_of ["ix"; "iy"; "sxx"; "sxy"; "syy"];
  !! fuse ~nest_of:2 ["ix"; "iy"];
  !! fuse ~nest_of:2 ["ixx"; "ixy"; "iyy"];
  !! fuse ~nest_of:2 ["sxx"; "sxy"; "syy"; "out"];
  let elim_matrix m = Matrix.elim ~var:m [cFunBody "harris"] in
  !! List.iter elim_matrix ["ixx"; "ixy"; "iyy"; "sxx"; "sxy"; "syy"];
  (* FIXME: Loop_basic.delete_void + on Matrix.elim ~simpl:delete_void_loops *)
  !! Instr.delete [occIndex ~nb:4 2; cFor "y"];

  bigstep "overlapped tiling over lines";
  (* following could be Loop.tile: *)
  (* let tile_size = 32 *)
  !!! Loop.slide ~size:(trm_int 32) ~step:(trm_int 32) [cFor ~body:[cArrayWrite "out"] "y"];
  !! Loop.slide ~size:(trm_int 34) ~step:(trm_int 32) [cFor ~body:[cArrayWrite "ix"] "y"];
  !! Loop.slide ~size:(trm_int 36) ~step:(trm_int 32) [cFor ~body:[cArrayWrite "gray"] "y"];
  !!! fuse ~index:"by" ["gray"; "ix"; "out"];

  bigstep "circular buffers";
  (* Without tiling:
  !! Image.loop_align_stop_extend_start "y" ~start:(trm_int 0) ~stop:(trm_var "h");
  *)
  !! Image.loop_align_stop_extend_start "y" ~start:(trm_var "by") ~stop:(expr "min(h, by + 36)");
  !!! Arith.(simpl_rec gather_rec) [];
  let rewrite rule = Rewrite.equiv_at ~ctx:true ~indepth:true rule [] in
  !!! List.iter rewrite [
    "int h; int by; ==> by + min(h, by + 36) - min(h - 2, by + 34) == by + 2";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 2, by + 34) == y - 2";
    "int h; int by; ==> by + min(h, by + 36) - min(h - 4, by + 32) == by + 4";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 4, by + 32) == y - 4";
  ];
  !! Arith.(simpl_rec gather_rec) [];
  (* min(h - 4, by + 32) - min(h, by + 36) *)
  !! fuse ["gray"; "ix"; "ixx"; "out"];
  (* FIXME: introduce local buffers inside tile loops
  let circular_buffer v = Matrix.storage_folding ~dim:0 ~size:(trm_int 3) ~var:v [cFunBody "harris"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];
*)
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