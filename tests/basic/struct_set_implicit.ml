open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  !! Struct_basic.set_implicit ~keep_label:true [cLabel "group1"];
  (* DONE: check that the wrapper handles the label with dBody;
     here, the label "group1" should disappear *)

  (* apply operations to multiple groups *)
  !! Trace.alternative (fun () ->
    Struct_basic.set_implicit [nbMulti; cLabel ~regexp:true "group."; dBody];
    !!(););

  (* apply operation using a more complex target *)
  !! Trace.alternative (fun () ->
    let tg = [cSeq ~args_pred:(Target.target_list_one_st [sInstr ~substr:false "b.x = p.x"]) ()] in
    Struct_basic.set_implicit tg;
    !!(););
)
