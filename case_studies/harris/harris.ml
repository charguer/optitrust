open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true

(* TODO: generalize *)
let%transfo simpl_mins ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Trace.step_atomic ();
  let rewrite rule = Rewrite.equiv_at ~simpl ~ctx:true ~indepth:true rule tg in
  List.iter rewrite [
    "int h; int by; ==> min(h, by + 36) - min(h - 2, by + 34) == 2";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 2, by + 34) == y - 2";
    "int h; int by; ==> min(h, by + 36) - min(h - 4, by + 32) == 4";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 4, by + 32) == y - 4";
    "int by; int h; int y; ===> y - min(h, by + 36) + by + min(h - 4, by + 32) == y + by - 4";
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
  let ctx = cFunBody "harris" in

  bigstep "fuse stencils with overlapped line tiling";
  !! Variable.inline ~simpl [ctx; multi cVarDef ["h1"; "w1"; "h2"; "w2"]];
  let fuse (ops, overlaps, outputs) = Stencil.fusion_targets ~nest_of:2 ~outputs ~overlaps [ctx; any cFun ops] in
  let overlaps_2x2 vars = List.map (fun i -> i,
    List.init 2 (fun _ -> trm_int 2)) vars in
  !! List.iter fuse [
    ["grayscale"], [], ["gray"];
    ["sobelX"; "sobelY"], [], ["ix"; "iy"];
    ["mul"; "sum3x3"; "coarsity"], overlaps_2x2 ["ixx"; "ixy"; "iyy"], ["out"];
  ];
  !! Stencil.fusion_targets_tile [int 32] ~outputs:["out"] ~overlaps:[
    "gray", [int 4]; "ix", [int 2]
  ] [ctx; nbMulti; cFor "y"];

  bigstep "use circular buffers";
  let circular_buffer var = Matrix.storage_folding ~dim:0 ~size:(int 4) ~var [ctx; cFor "y"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];

  bigstep "code details";
  !! simpl_mins [ctx]; (* TODO: should this be done by Stencil.fusion_targets? *)
  !! Matrix.elim [ctx; multi cVarDef ["ixx"; "ixy"; "iyy"]];
  !! Loop.unroll ~nest_of:2 [ctx; nbMulti; cFor ~body:[cPlusEq [cVarReg "acc_.*"]] "i"];
  (* FIXME: need Matrix.inline_constant to avoid deleting functions *)
  !! Function.delete [multi cFunDef ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "conv2D"; "mul"; "coarsity"]];
  !! Matrix.elim_constant ~simpl:simpl_inplace_noop [nbMulti; cVarDefReg "weights.*"];
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[ctx; tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [ctx; cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  !! Matrix.elim_mops [ctx];

  bigstep "parallelism";
  !! Omp.simd ~clause:[Simdlen 8] [ctx; nbMulti; cFor "x"];
  !! Omp.parallel_for [ctx; cFor "y"];
)