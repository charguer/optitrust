open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  
  !! Record_basic.set_implicit ~keep_label:false [cLabel "fuse"];
  
)

"
typedef struct {
  int x;
  int y;
} vect;

int main() {
  vect a;
  vect b;
  fuse:{
    a.x = b.x;
    a.y = b.y;
  }
}
"
(* LATER: rename ~keep_label:true to ~rem_label:false, which would be the default (as it is now) *)

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.set_implicit ~keep_label:true [cLabel "group1"];
  (* DONE: check that the wrapper handles the label with dBody;
     here, the label "group1" should disappear *)

  (* apply operations to multiple groups *)
  !! Trace.alternative (fun () ->
    Record_basic.set_implicit [nbMulti; cLabel ~regexp:true "group."];
    !!(););

  (* apply operation using a more complex target *)
  !! Trace.alternative (fun () ->
    let tg = [cSeq ~args_pred:(Target.target_list_one_st [cFieldWrite ~base:[cVar "b"] ()]) ()] in
    !! Record_basic.set_implicit tg;
    !!(););

)
