open Optitrust
open Prelude

(** [loop_single_on i t] : Expect an index and a sequence trm t, i-th trm of the
    sequence should point to a var def.
    Replace
      int k = ..
      tl
    with
      for(int k = .. ; k < .. +1 ; k++)
      {
        tl
      }

*)
let loop_single_on (i : int) (t : trm) : trm =
  let seq, result =
    trm_inv ~error:"Loop_single_on: Expected the target to be part of a Seq"
      trm_seq_inv t
  in
  let let_stmt = Mlist.nth seq i in
  let var, typ, value =
    trm_inv
      ~error:
        "Loop_single_on: Expected the target to be a let operation (int .. = \
         ...;)"
      trm_let_inv let_stmt
  in
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

  match result with
  | Some res_expr ->
      let mut_res_var = new_var "res" in
      let mut_res = trm_let_mut_uninit (mut_res_var, typ_int) in
      let new_body =
        trm_seq_helper
          [
            TrmMlist (Mlist.pop_front tl2);
            Trm (trm_set (trm_var mut_res_var) (trm_var res_expr));
          ]
      in
      let loop = trm_for l_range new_body in
      let result_v = new_var "__res" in
      let result_p = trm_let (result_v, typ_int) (trm_var_get mut_res_var) in
      trm_seq_helper ~result:result_v
        [ TrmMlist (Mlist.push_back mut_res tl1); Trm loop; Trm result_p ]
  | None ->
      let loop = trm_for l_range (trm_seq (Mlist.pop_front tl2)) in
      trm_seq_helper [ TrmMlist tl1; Trm loop ]

let loop_single tg = (apply_at_target_paths_in_seq loop_single_on) tg

let _ =
  Run.script_cpp (fun () ->
      !!loop_single [ cFunBody "main"; cVarDef "k" ];
      !!loop_single [ cFunBody "main2"; cVarDef "k" ];
      !!loop_single [ cFunBody "main3"; cVarDef "k" ])
