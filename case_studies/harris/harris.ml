open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true

(* TODO: generalize *)
let%transfo simpl_mins ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Trace.step_atomic ();
  let rewrite rule = Rewrite.equiv_at ~simpl ~ctx:true ~indepth:true rule tg in
  List.iter rewrite [
    "int h; int by; ==> by + min(h, by + 36) - min(h - 2, by + 34) == by + 2";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 2, by + 34) == y - 2";
    "int h; int by; ==> by + min(h, by + 36) - min(h - 4, by + 32) == by + 4";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 4, by + 32) == y - 4";
  ]

(* TODO: generalize *)
let%transfo simpl_inplace_noop (tg : target) : unit =
  Trace.step_atomic ();
  Target.iter (fun _ p ->
    let surrounding_instr_p = Loop.find_surrounding_instr p (Trace.ast ()) in
    Arith.default_simpl (target_of_path p);
    let surrounding_instr = (target_of_path surrounding_instr_p) in
    Instr.delete (surrounding_instr @ [nbAny; sInstr "+= 0"]);
  ) tg

let _ = Run.script_cpp (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in

  bigstep "fuse stencils with overlapped tiling over lines";
  !! Variable.inline ~simpl [multi cVarDef ["h1"; "w1"; "h2"; "w2"]];
  let fuse (ops, overlaps, outputs) = Stencil.fusion_targets ~nest_of:2 ~outputs ~overlaps [any cFun ops] in
  let overlaps_2x2 vars = List.map (fun i -> i,
    List.init 2 (fun _ -> trm_int 2)) vars in
  !! List.iter fuse [
    ["grayscale"], [], ["gray"];
    ["sobelX"; "sobelY"], [], ["ix"; "iy"];
    ["mul"; "sum3x3"; "coarsity"], overlaps_2x2 ["ixx"; "ixy"; "iyy"], ["out"];
  ];
  !! Function.delete [multi cFunDef ["grayscale"; "sobelX"; "sobelY"; "mul"; "sum3x3"; "coarsity"; "conv2D"]];

  (* TODO: move into details *)
  !! Matrix.elim [multi cVarDef ["ixx"; "ixy"; "iyy"]];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEq [cVarReg "acc_.*"]] "i"];

  !! Stencil.fusion_targets_tile [int 32] ~outputs:["out"] ~overlaps:[
    "gray", [int 4]; "ix", [int 2]
  ] [nbMulti; cFunBody "harris"; cFor "y"];


  (* TODO: stopped here *)
  bigstep "circular buffers";
  !! Stencil.loop_align_stop_extend_start_like ~orig:[cFor "y_gray" ~body:[cArrayWrite "gray"]] [nbMulti; cFor "" ~body:[cStrict; cFor "x"; any cArrayWrite ["ix"; "out"]]];
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
  let circular_buffer var = Matrix.storage_folding ~dim:0 ~size:(int 4) ~var [cFunBody "harris"; cFor "by"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];

  bigstep "code details";
  !! Matrix.elim_constant ~simpl:simpl_inplace_noop [nbMulti; cVarDefReg "weights.*"];
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