open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true
(* let _ = Flags.analyse_stats := true *)

(* keep local *)
module Image = struct
  let loop_align_stop_extend_start (index : var) ~(start : trm) ~(stop : trm) : unit =
    Loop.shift (StopAt stop) [nbMulti; cFor index];
    Loop.extend_range ~start:(ExtendTo start) [nbMulti; cFor index];
    (* FIXME: hack to trigger missed simplifications *)
    Trace.reparse ();
    Arith.(simpl_rec gather_rec) [nbMulti; cFor index];
    (* TODO: transfo to remove useless if inside a for *)
end

let _ = Run.script_cpp (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in

  bigstep "inline operators";
  (* Function.inline_def *)
  !! Function.inline ~delete:true [nbMulti; cFun "conv2D"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_constant [nbMulti; cVarDef "weights"];
  !! Function.inline ~delete:true [multi cFun ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "mul"; "coarsity"]];
  !! Variable.inline ~simpl [multi cVarDef ["h1"; "w1"; "h2"; "w2"]];

  bigstep "fuse operators";
  let rename_acc_of array = Variable.rename ~into:("acc_" ^ array) [cFor ~body:[cArrayWrite array] ""; cVarDef "acc"] in
  !! List.iter rename_acc_of ["ix"; "iy"; "sxx"; "sxy"; "syy"];
  let fuse arrays = Loop.fusion_targets ~nest_of:2 [cFor "y" ~body:[any cArrayWrite arrays]] in
  !! List.iter fuse [["ix"; "iy"]; ["ixx"; "ixy"; "iyy"]; ["sxx"; "sxy"; "syy"; "out"]];
  !! Matrix.elim [multi cVarDef ["ixx"; "ixy"; "iyy"; "sxx"; "sxy"; "syy"]];

  bigstep "overlapped tiling over lines";
  let tile_size = 32 in
  let slide overlap = Loop.slide ~size:(int (tile_size + overlap)) ~step:(int tile_size) in
  !!! slide 0 [cFor "y" ~body:[cArrayWrite "out"]];
  !! slide 2 [cFor "y" ~body:[cArrayWrite "ix"]];
  !! slide 4 [cFor "y" ~body:[cArrayWrite "gray"]];
  !!! Loop.fusion_targets [cFor "by" ~body:[any cArrayWrite ["gray"; "ix"; "out"]]];

  bigstep "circular buffers";
  (* TODO: Image.loop_align_stop_extend_start ~like:[cFor "y" ~body:[cArrayWrite "gray"]] [cFor "y" ~body:[any cArrayWrite ["ix"; "gray"]]] *)
  !! Image.loop_align_stop_extend_start "y" ~start:(trm_var "by") ~stop:(expr "min(h, by + 36)");
  let rewrite rule = Rewrite.equiv_at ~simpl ~ctx:true ~indepth:true rule [] in
  !!! List.iter rewrite [
    "int h; int by; ==> by + min(h, by + 36) - min(h - 2, by + 34) == by + 2";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 2, by + 34) == y - 2";
    "int h; int by; ==> by + min(h, by + 36) - min(h - 4, by + 32) == by + 4";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 4, by + 32) == y - 4";
  ];
  !! Loop.fusion_targets [cFor "y" ~body:[any cArrayWrite ["gray"; "ix"; "ixx"; "out"]]];
  let local_matrix (m, tile) =
    let tmp_m = ("l_" ^ m) in
    Matrix.local_name_tile m ~into:tmp_m ~alloc_instr:[cVarDef m] ~indices:["y"; "x"] tile [cFunBody "harris"; cFor ~body:[cArrayWrite m] "y"];
    (* FIXME: dangerous transformation? *)
    (* TODO: Matrix.delete_not_read + Loop.delete_void
       - how close to Matrix.elim is this? *)
    Instr.delete [occFirst; cFor "y" ~body:[cArrayRead m]];
    Instr.delete [occLast; cFor "y" ~body:[cArrayWrite m]];
    (* Loop.delete_all_void [cFor "by"]; *)
    Matrix.delete ~var:m [cFunBody "harris"];
    Variable.rename ~into:m [cVarDef tmp_m];
  in
  !! List.iter local_matrix [
    ("gray", [(expr "by", int 36); (int 0, expr "w")]);
    ("ix", [(expr "by", int 34); (int 0, expr "w - 2")]);
    ("iy", [(expr "by", int 34); (int 0, expr "w - 2")]);
  ];
  (* TODO: local_name_tile ~simpl *)
  !! Arith.(simpl_rec gather) [];
  let circular_buffer v = Matrix.storage_folding ~dim:0 ~size:(int 4) ~var:v [cFunBody "harris"; cFor "by"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];

  bigstep "code details";
  !!! Loop.shift StartAtZero [cFor "y"];
  !!! Instr.delete [multi sInstr ["0.f *"; "* 0.f"]];
  (* !!! List.iter rewrite [
    "int a; int b; int c; ==> (a + b <= c + b) == (a <= b)";
  ]; *)
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  !! Matrix.elim_mops [];

  bigstep "parallelism";
  !! Omp.header ();
  !! Omp.simd ~clause:[Simdlen 8] [nbMulti; cFor "x"];
  !! Omp.parallel_for [cFor "by"];
)