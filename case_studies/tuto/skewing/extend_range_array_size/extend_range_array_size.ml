open Optitrust
open Prelude

(** [extend_range_array_size ?dim orig tg] Extends the iteration range of a loop
    to cover the full length of the array pointed to by [orig].

    This function expects [tg] to point to a for loop.

    @param dim:
      the index of the dimension to consider when accessing the array's size. *)
let extend_range_array_size ?(dim : int = 0) (orig : target) (tg : target) =
  let array_ref = get_trm_at_exn orig in
  let error = "Extend_range_array_size: Expected an array declaration" in
  let _, array = trm_inv ~error trm_ref_inv array_ref in
  let error = "Extend_range_array_size: Expected use of MALLOC function" in
  let _ty, dims, _zero_init = trm_inv ~error Matrix_trm.alloc_inv array in
  let dim_to_consider = List.nth dims dim in
  Loop.extend_range ~start:ExtendToZero ~stop:(ExtendTo dim_to_consider) tg

let _ =
  Run.script_cpp (fun _ ->
      (* !!Matrix.insert_alloc_dim (expr "N2") [ cVarInit "p" ];*)
      !!extend_range_array_size
        [ cFunBody "main"; cVarInit "p" ]
        [ cFunBody "main"; cFor "i" ];
      !!extend_range_array_size ~dim:1
        [ cFunBody "main2"; cVarInit "p" ]
        [ cFunBody "main2"; cFor "i" ])
