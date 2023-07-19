open Optitrust
open Target
open Syntax

let _ = Flags.pretty_matrix_notation := true

module Image = struct
  let loop_align_stop_extend_start ~(start : trm) ~(stop : trm) (tg : target) : unit =
    Loop.shift (StopAt stop) tg;
    Loop.extend_range ~start:(ExtendTo start) tg;
    Trace.reparse ();
    Arith.(simpl_rec gather_rec) tg

  let loop_align_stop_extend_start_like ~(orig:target) (tg:target) : unit =
    let t = get_trm_at_exn orig in
    let error = "Image.loop_align_stop_extend_start_like: expected simple loop" in
    let ((_index, start, _dir, stop, _step, _par), _body) = trm_inv ~error trm_for_inv t in
    loop_align_stop_extend_start ~start ~stop tg
end

(* TODO: generalize *)
let%transfo simpl_mins ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Trace.tag_atomic ();
  let rewrite rule = Rewrite.equiv_at ~simpl ~ctx:true ~indepth:true rule tg in
  List.iter rewrite [
    "int h; int by; ==> by + min(h, by + 36) - min(h - 2, by + 34) == by + 2";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 2, by + 34) == y - 2";
    "int h; int by; ==> by + min(h, by + 36) - min(h - 4, by + 32) == by + 4";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 4, by + 32) == y - 4";
  ]

(* TODO: generalize *)
let%transfo simpl_inplace_noop (tg : target) : unit =
  Trace.tag_atomic ();
  Target.iter (fun _ p ->
    let surrounding_instr_p = Loop.find_surrounding_instr p (Trace.ast ()) in
    Arith.default_simpl (target_of_path p);
    let surrounding_instr = (target_of_path surrounding_instr_p) in
    Instr.delete (surrounding_instr @ [nbAny; sInstr "+= 0"]);
  ) tg

let _ = Run.script_cpp ~filename:"harris.cpp" (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in

  bigstep "inline operators";
  !! Function.inline_def ~simpl [cFunDef "conv2D"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEq [cVar "acc"]] "i"];
  !! Matrix.elim_constant ~simpl:simpl_inplace_noop [nbMulti; cVarDefReg "weights.*"];
  !! Function.inline_def ~simpl [multi cFunDef ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "mul"; "coarsity"]];
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
  !! Image.loop_align_stop_extend_start_like ~orig:[cFor "y" ~body:[cArrayWrite "gray"]] [nbMulti; cFor "y" ~body:[any cArrayWrite ["ix"; "out"]]];
  !! simpl_mins [];
  !! Loop.fusion_targets [cFor "y" ~body:[any cArrayWrite ["gray"; "ix"; "ixx"; "out"]]];
  let local_matrix (m, tile) =
    Matrix.local_name_tile m ~simpl ~alloc_instr:[cVarDef m] tile [cFunBody "harris"; cFor ~body:[cArrayWrite m] "y"]
  in
  !! List.iter local_matrix [
    ("gray", [(expr "by", int 36); (int 0, expr "w")]);
    ("ix", [(expr "by", int 34); (int 0, expr "w - 2")]);
    ("iy", [(expr "by", int 34); (int 0, expr "w - 2")]);
  ];
  !! Matrix.storage_folding ~dim:0 ~size:(int 4) [cFunBody "harris"; multi cVarDef ["gray"; "ix"; "iy"]];

  bigstep "code details";
  !!! Loop.shift StartAtZero [cFor "y"];
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  !! Matrix.elim_mops [];

  bigstep "parallelism";
  !! Omp.simd ~clause:[Simdlen 8] [nbMulti; cFor "x"];
  !! Omp.parallel_for [cFor "by"];
)