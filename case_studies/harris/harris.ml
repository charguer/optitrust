open Optitrust
open Prelude

let _ = Flags.pretty_matrix_notation := true

(* TODO: generalize *)
let%transfo simpl_mins ?(simpl : target -> unit = Arith.default_simpl) (tg : target) : unit =
  Scope.infer_var_ids ();
  Trace.tag_atomic ();
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
  Trace.tag_atomic ();
  Target.iter (fun p ->
    let surrounding_instr_p = Loop.find_surrounding_instr p (Trace.ast ()) in
    Arith.default_simpl (target_of_path p);
    let surrounding_instr = (target_of_path surrounding_instr_p) in
    Instr.delete (surrounding_instr @ [nbAny; sInstr "+= 0"]);
  ) tg

let _ = Run.script_cpp (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in
  let ctx = cFunBody "harris" in

  !! Variable.inline ~simpl [ctx; multi cVarDef ["h1"; "w1"; "h2"; "w2"]];
  let fuse (ops, overlaps, outputs) =
    Stencil.fusion_targets ~nest_of:2 ~outputs ~overlaps [ctx; any cFun ops] in
  let overlaps_2x2 vars = List.map (fun i -> i, [int 2; int 2]) vars in
  !! List.iter fuse [
    ["grayscale"], [], ["gray"];
    ["sobelX"; "sobelY"], [], ["ix"; "iy"];
    ["mul"; "sum3x3"; "coarsity"], overlaps_2x2 ["ixx"; "ixy"; "iyy"], ["out"];
  ];
  !! Stencil.fusion_targets_tile [int 32] ~outputs:["out"]
     ~overlaps:["gray", [int 4]; "ix", [int 2]]
     [ctx; nbMulti; cFor "y"];
  !! simpl_mins [ctx];
  !! Matrix.storage_folding ~dim:0 ~size:(int 4) [ctx; multi cVarDef ["gray"; "ix"; "iy"]];
  !! Matrix.elim [ctx; multi cVarDef ["ixx"; "ixy"; "iyy"]];
  let inline v = Matrix.inline_constant ~simpl:simpl_inplace_noop ~decl:[cVarDef v] [ctx; nbMulti; cArrayRead v] in
  !! List.iter inline ["weights_sobelX"; "weights_sobelY"; "weights_sum3x3"];
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[ctx; tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [ctx; cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  !! Matrix.elim_mops [ctx];
  !! Omp.parallel_for [ctx; cFor "y"];
  !! Omp.simd ~clause:[Simdlen 8] [ctx; nbMulti; cFor "x"];


)
