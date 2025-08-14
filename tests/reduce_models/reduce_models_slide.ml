open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp(fun _ ->
  !! ();
(*
  let matrix_s = trm_find_var "s" [cFunBody "rowSum"] in
  let n = trm_find_var "n" [cFunBody "rowSum"] in
  let w = trm_find_var "w" [cFunBody "rowSum"] in
  let cn = trm_find_var "cn" [cFunBody "rowSum"] in
  let matrix_s_dims = [trm_sub_int (trm_add_int n w) (trm_int 1); cn] in
  let c = trm_find_var "c" [cFunBody "rowSum"] in
  let k_compute_f_elem k con =
    let open Resource_formula in
    let open Resource_trm in

    let in_range_ghosts = List.map2 (fun i d ->
      Resource_trm.to_prove (formula_in_range i (formula_range (trm_int 0) d (trm_int 1)))
    ) [k; c] matrix_s_dims in
    let (_, focus, unfocus) = ghost_pair (ghost_ro_matrix2_focus ~matrix:matrix_s k c) in
    in_range_ghosts @ [focus] @ con(Matrix_trm.get matrix_s matrix_s_dims [k; c]) @ [unfocus]
  in
*)

  !! Reduce_models.slide [cFunBody "rowSum"; cFor "i"];
)
