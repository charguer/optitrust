


let _ = Run.script_cpp (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in

  bigstep "inline operators";
  !! Function.inline_local_funs [cFunDef "harris"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_constant [nbMulti; cVarDef "weights"];
  !! Variable.inline [multi cVarConst ()];

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
    Matrix.local_name_tile m ~alloc_instr:[cVarDef m] tile [cFunBody "harris"; cFor ~body:[cArrayWrite m] "y"]
  in
  !! List.iter local_matrix [
    ("gray", [(expr "by", int 36); (int 0, expr "w")]);
    ("ix", [(expr "by", int 34); (int 0, expr "w - 2")]);
    ("iy", [(expr "by", int 34); (int 0, expr "w - 2")]);
  ];
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