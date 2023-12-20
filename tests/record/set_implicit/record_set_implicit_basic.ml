open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->

  !! Record_basic.set_implicit ~keep_label:true [cLabel "group1"];
  (* DONE: check that the wrapper handles the label with dBody;
     here, the label "group1" should disappear *)

  (* apply operations to multiple groups *)
  !! Trace.restore_original();
  !! Record_basic.set_implicit [nbMulti; cLabel ~regexp:true "group."];

  (* apply operation using a more complex target *)
  !! Trace.restore_original();
  let tg = [cSeq ~args_pred:(Target.target_list_one_st [cFieldWrite ~base:[cVar "b"] ()]) ()] in
  !! Record_basic.set_implicit tg;

)
