open Optitrust
open Prelude

let _ = Flags.check_validity := false

let loop_single_on (i : int) (t : trm) : trm =
  let error = "Final_name: Expected the target to be in a seq" in
  let seq, result = trm_inv ~error trm_seq_inv t in
  let index = Mlist.nth seq i in
  let error =
    "Final name: Expected the target to be a let operation (int .. = ...;)"
  in
  let var, typ, value = trm_inv ~error trm_let_inv index in
  let tl1, _mark, tl2 = Mlist.split_on_marks i seq in
  let l_range : loop_range =
    {
      index = var;
      start = value;
      stop = trm_add_int value (trm_int 1);
      direction = DirUp;
      step = trm_step_one ();
    }
  in

  let loop = trm_for l_range (trm_seq (Mlist.pop_front tl2)) in
  trm_seq (Mlist.push_back loop tl1)

let loop_single tg = (apply_at_target_paths_in_seq loop_single_on) tg

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
      !!Variable_basic.bind ~const:true "k"
        [
          cForBody "i";
          cWrite ~lhs:[ cVar "c" ] ();
          cBinop ~lhs:[ cVar "i" ] Binop_add;
        ];
      !!loop_single [ cVarDefReg "k" ];
      !!extend_range_array_size [ cVarInit "c" ] [ cFor "k" ];
      !!Rewrite.equiv_at
        "const int i;const int j;const int k ==> (i + j <= k && k < i + j + 1) == (j <= k-i)"
        [ cIf () ])
