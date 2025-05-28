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
  let error = "Final_name: Expected the target to be in a seq" in
  let seq, result = trm_inv ~error trm_seq_inv t in
  let index = Mlist.nth seq i in
  let error =
    "Final name: Expected the target to be a let operation (int .. = ...;)"
  in
  let var, typ, ref_value = trm_inv ~error trm_let_inv index in
  let typ, value = trm_inv ~error trm_ref_inv ref_value in
  if not (are_same_trm typ typ_int) then
    trm_fail t "Final name: Expected an int";
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

let _ =
  Run.script_cpp (fun () ->
  !!loop_single [ cFunBody "main"; cVarDef "k" ];
  !!loop_single [ cFunBody "main2"; cVarDef "k" ];
  !!loop_single [ cFunBody "main3"; cVarDef "k" ];
  )

