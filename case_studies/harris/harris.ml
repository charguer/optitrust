open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true
(* let _ = Flags.analyse_stats := true *)

(* keep local *)
module Image = struct
  let loop_align_stop_extend_start ~(start : trm) ~(stop : trm) (tg : target) : unit =
    Loop.shift (StopAt stop) tg;
    Loop.extend_range ~start:(ExtendTo start) tg;
    (* FIXME: hack to trigger missed simplifications *)
    Trace.reparse ();
    Arith.(simpl_rec gather_rec) tg
    (* TODO: transfo to remove useless if inside a for *)

      (*
  let loop_align_stop_extend_start_like ~(orig:target) (tg:target) : unit =
    let t = get_trm_at_exn orig in
    let () = trm_inv ~error trm_for_inv in
    loop_align_stop_extend_start ~start ~stop ~tg
    *)
end

let simpl_mins ?(simpl = Arith.default_simpl) (tg : target) : unit =
  let rewrite rule = Rewrite.equiv_at ~simpl ~ctx:true ~indepth:true rule tg in
  List.iter rewrite [
    "int h; int by; ==> by + min(h, by + 36) - min(h - 2, by + 34) == by + 2";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 2, by + 34) == y - 2";
    "int h; int by; ==> by + min(h, by + 36) - min(h - 4, by + 32) == by + 4";
    "int h; int y; int by; ==> y - min(h, by + 36) + min(h - 4, by + 32) == y - 4";
  ]

let _ = Run.script_cpp (fun () ->
  let simpl = Arith.default_simpl in
  let int = trm_int in

  bigstep "inline operators";
  (* Function.inline_def *)
  (* TODO: make Function.inline ~simpl work *)
  !! Function.inline ~simpl ~delete:true [nbMulti; cFun "conv2D"];
  !! Loop.unroll ~nest_of:2 [nbMulti; cFor ~body:[cPlusEqVar "acc"] "i"];
  !! Matrix.elim_constant [nbMulti; cVarDef "weights"];
  (* elim_constant ~simpl: simpl arith '* 0.0f' + simpl 'acc += 0.0' / simpl_inplace_noop *)
  !! Function.inline ~simpl ~delete:true [multi cFun ["grayscale"; "sobelX"; "sobelY"; "sum3x3"; "mul"; "coarsity"]];
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
  (* TODO: Image.loop_align_stop_extend_start ~like:[cFor "y" ~body:[cArrayWrite "gray"]] [cFor "y" ~body:[any cArrayWrite ["ix"; "out"]]] *)
  !! Image.loop_align_stop_extend_start ~start:(trm_var "by") ~stop:(expr "min(h, by + 36)") [nbMulti; cFor "y" ~body:[any cArrayWrite ["ix"; "out"]]];
  !! simpl_mins [];
  !! Loop.fusion_targets [cFor "y" ~body:[any cArrayWrite ["gray"; "ix"; "ixx"; "out"]]];
  let local_matrix (m, tile) = (* TODO remove ~indices *)
    Matrix.local_name_tile m ~alloc_instr:[cVarDef m] ~indices:["y"; "x"] tile [cFunBody "harris"; cFor ~body:[cArrayWrite m] "y"]
  in (* LATER: ("gray", [Some (expr "by", int 36);  None]);  *)
    (* LATER: ("gray", [(dim1, (expr "by", int 36))]);  *)
  !! List.iter local_matrix [
    ("gray", [(expr "by", int 36); (int 0, expr "w")]);
    ("ix", [(expr "by", int 34); (int 0, expr "w - 2")]);
    ("iy", [(expr "by", int 34); (int 0, expr "w - 2")]);
  ];
  (* TODO: local_name_tile ~simpl *)
  !! simpl [];
  let circular_buffer var = Matrix.storage_folding ~dim:0 ~size:(int 4) ~var [cFunBody "harris"; cFor "by"] in
  !! List.iter circular_buffer ["gray"; "ix"; "iy"];
  (* LATER: (a - 4 + b)%4 =  (a+b)%4 avec analyse statique *)

  bigstep "code details";
  !!! Loop.shift StartAtZero [cFor "y"];
  !!! Instr.delete [multi sInstr ["0.f *"; "* 0.f"]]; (* LATER: disappears earlier *)
  (* LATER !!! List.iter rewrite [
    "int a; int b; int c; ==> (a + b <= c + b) == (a <= b)";
  ]; *)
  let bind_gradient name =
    Variable.bind_syntactic ~dest:[tBefore; cVarDef "acc_sxx"] ~fresh_name:(name ^ "${occ}") [cArrayRead name]
  in
  !!! List.iter bind_gradient ["ix"; "iy"];
  !! Matrix.elim_mops [];

  bigstep "parallelism";
  !! Omp.header (); (* should be implicit *)
  !! Omp.simd ~clause:[Simdlen 8] [nbMulti; cFor "x"];
  !! Omp.parallel_for [cFor "by"];
)