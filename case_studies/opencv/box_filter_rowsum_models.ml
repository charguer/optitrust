open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

let _ = Flags.save_steps := Some Flags.Steps_effectful
let _ = Flags.save_ast_for_steps := Some Flags.Steps_effectful
(*
*)
(* Generated trace is too heavy. Keep only the steps of the transformation script
let _ = Flags.save_steps := Some Steps_script
 *)

(** Reproducing OpenCV code from:
  https://github.com/opencv/opencv/blob/4.10.0/modules/imgproc/src/box_filter.simd.hpp#L75

   Remaining differences:
   - [k++; + k] instead of [k++, S++, D++], we may not want to introduce such pointer arithmetic.
   - no template support yet for S/ST types; OpenCV also casts inputs from uchar

   c.f. README.md
   *)

let int = trm_int

let custom_specialize_simpl tg =
  Trace.without_resource_computation_between_steps (fun () ->
    Arith.default_simpl tg;
    Loop.simplify_all_ghosts_group_scale [];
  )

let no_simpl (tg : target) : unit = ()

let _ = Run.script_cpp (fun () ->
  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"anyw"
    ["w", int 3; "w", int 5] [cFunBody "rowSum"; cFor "i"];
  (* TODO:
    !! Reduce.elim ~inline:true [nbMulti; cMark "w"; cCall "reduce_spe1"];

    is following unroll + fold adds
  !! Loop.unroll [nbMulti; cMark "w"; cFor "k"];
  *)
  !! Loop.collapse [nbMulti; cMark "w"; cFor "i"];

  (* TODO: infer code below *)
  let matrix_s = trm_find_var "s" [cMark "anyw"] in
  let n = trm_find_var "n" [cMark "anyw"] in
  let w = trm_find_var "w" [cMark "anyw"] in
  let cn = trm_find_var "cn" [cMark "anyw"] in
  let matrix_s_dims = [trm_sub_int (trm_add_int n w) (trm_int 1); cn] in
  let c = trm_find_var "c" [cMark "anyw"; cPlusEq ()] in
  let k_compute_f_elem k con =
    let open Resource_formula in
    let open Resource_trm in

    let in_range_ghosts = List.map2 (fun i d ->
      Resource_trm.to_prove (formula_in_range i (formula_range (trm_int 0) d (trm_int 1)))
    ) [k; c] matrix_s_dims in
    let (_, focus, unfocus) = ghost_pair (ghost_ro_matrix2_focus ~matrix:matrix_s k c) in
    in_range_ghosts @ [focus] @ con(Matrix_trm.get matrix_s matrix_s_dims [k; c]) @ [unfocus]
  in

  !! Loop.swap [nbMulti; cMark "anyw"; cFor "i"];
  (* TODO: would need to make slide less syntax-driven to enable simpl again *)
  !! Loop.unroll_first_iteration ~simpl:no_simpl [nbMulti; cMark "anyw"; cFor "i"];
  !! Reduce_models.slide ~mark_alloc:"acc" k_compute_f_elem [nbMulti; cMark "anyw"; cFor "i"];
  (* TODO: do that in slide combi *)
  !! Resources.make_strict_loop_contracts [nbMulti; cMark "anyw"; cFor "i"];

  !! Variable.elim_reuse [nbMulti; cMark "acc"];
  (* !! Loop.shift_range (StartAtZero) [nbMulti; cMark "anyw"; cFor "i"];
  !! Loop.scale_range ~factor:(trm_find_var "cn" []) [nbMulti; cMark "anyw"; cFor "i"]; *)
  !! Specialize.variable_multi ~mark_then:fst ~mark_else:"anycn" ~simpl:custom_specialize_simpl
    ["cn", int 1; "cn", int 3; "cn", int 4] [cMark "anyw"; cFor "c"];

  !! Loop.unroll [nbMulti; cMark "cn"; cFor "c"];
  !! Target.foreach [nbMulti; cMark "cn"] (fun c ->
    Printf.printf "next cn fusions\n";
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "i"];
    Instr.gather_targets [c; cStrict; cArrayWrite "d"];
    Loop.fusion_targets ~into:FuseIntoLast [nbMulti; c; cFor "k"];
    Instr.gather_targets [c; cFor "i"; cArrayWrite "d"];
  );
  (*
  !! Loop.shift_range (ShiftBy (trm_find_var "c" [cMark "anycn"])) [cMark "anycn"; cFor ~body:[cArrayWrite "D"] "i"];
*)
)

