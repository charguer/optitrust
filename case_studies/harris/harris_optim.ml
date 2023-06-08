


let _ = Run.script_cpp (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in

  bigstep "fuse operators";
  (* implicit 'fuse_ops'
     !! Function.inline_def ([cTopFunDef ""] *t [cFunBody "harris"; cFun ""]);
     !! Function.inline [nbMulti; [cTopFunDef ""] -t [cFunDef "harris"]] *)
  (* implicit: !! Variable.unique_name [nbMulti; cVarDef "acc"]; *)
  let fuse ops = Stencil.fuse_operators ~nest_of:2 [any cFun ops] in
  !! List.iter fuse [["sobelX"; "sobelY"]; ["mul"; "sum3x3", "coarsity"]];
  (* implicit: !! Matrix.elim [multi cVarDef ["ixx"; "ixy"; "iyy"; "sxx"; "sxy"; "syy"]]; *)

  bigstep "???";
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_constant [nbMulti; cVarDef "weights"];
  !! Variable.inline [cScalarVars ()];

  bigstep "overlapped tiling over lines";
  !! Stencil.overlapped_tiling ~tile_size:32 ~overlaps:[2; 4]
    [cFor "y" ~body:[any cArrayWrite ["out", "ix", "gray"]]];
  (* implicit:
  let slide overlap = Loop.slide ~size:(int (tile_size + overlap)) ~step:(int tile_size) in
  !!! slide 0 [cFor "y" ~body:[cArrayWrite "out"]];
  !! slide 2 [cFor "y" ~body:[cArrayWrite "ix"]];
  !! slide 4 [cFor "y" ~body:[cArrayWrite "gray"]];
  !!! Loop.fusion_targets [cFor "by" ~body:[any cArrayWrite ["gray"; "ix"; "out"]]];
  !! Image.loop_align_stop_extend_start_like [nbMulti; cFor "y"];
  !! simpl_mins [];
  !! Loop.fusion_targets [cFor "y" ~body:[any cArrayWrite ["gray"; "ix"; "ixx"; "out"]]];
  let local_matrix (m, tile) =
    Matrix.local_name_tile m ~alloc_instr:[cVarDef m] tile [cFunBody "harris"; cFor ~body:[cArrayWrite m] "y"]
  in
  !! List.iter local_matrix [
    ("gray", [(expr "by", int 36); (int 0, expr "w")]);
    ("ix", [(expr "by", int 34); (int 0, expr "w - 2")]);
    ("iy", [(expr "by", int 34); (int 0, expr "w - 2")]);
  ];
  *)

  bigstep "circular buffers";
  let circular_buffer var = Matrix.storage_folding ~dim:0 ~size:(int 4) ~var [cFunBody "harris"; cFor "by"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];

  bigstep "code details";
  !!! Loop.shift StartAtZero [cFor "y"];
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  !! Matrix.elim_mops [];

  bigstep "parallelism";
  !! Omp.simd [nbMulti; cFor "x"];
  !! Omp.parallel_for [cFor "by"];
)