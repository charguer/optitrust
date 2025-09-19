open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.disable_stringreprs := true

(* Generated trace is too heavy. Keep only the steps of the transformation script *)
let _ = Flags.save_steps := Some Steps_script

let _ = Run.script_cpp (fun () ->
  !! Loop.unroll [nbMulti; (* cMark "w"; *) cFor "ic"; cFor "k"];
  !! Target.foreach [nbMulti; cFor "ic"] (fun c ->
    Instr.accumulate_targets [c; cPlusEq ()]);
  (* TODO: inline sum *)

  !! Loop.shift_range (StartAtZero) [nbMulti; (* cMark "anyw"; *) cFors ["k"; "i"]];

  (* !! Loop.scale_range ~factor:(trm_find_var "cn" []) [nbMulti; (* cMark "anyw"; *) cFors ["k"; "i"]]; *)
  let scale_cn cn tg_base = Target.foreach tg_base (fun c ->
    Loop.scale_range ~factor:cn [nbMulti; c; cFors ["k"; "i"]];
  ) in
  !! scale_cn (trm_int 1) [cThen; occIndex 2];
  !! scale_cn (trm_int 3) [cThen; occIndex 3];
  !! scale_cn (trm_int 4) [cThen; occIndex 4];
  !! scale_cn (trm_find_var "cn" []) [cFor "c"];

  !! Loop.shift_range (ShiftBy (trm_find_var "c" [(*cMark "anycn"*) cFor "c"])) [cFor "c" (* cMark "anycn" *); cFor (* ~body:[cArrayWrite "D"]*) "i"];

  !! (fun _ ->
    Marks.rem_all_marks_rec [];
    Function.use_infix_ops ~indepth:true [];
    Matrix.elim_mops ~simpl:(fun t -> t) [];
    Arith.(simpl_rec expand_rec) [];
    Arith.(simpl_rec (compose [euclidian; compute])) [];
    Arith.(simpl_rec gather_rec) [];
    Arith.(simpl_rec compute) [];
    Arith.(simpl2_rec sort) [];
    Resources.delete_annots [];
    Loop.delete_all_void []
  ) [];
)

