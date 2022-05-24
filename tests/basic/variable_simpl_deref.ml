open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.simpl_deref ~indepth:true [dRoot];
  )
"
int main() {
  int a = 1;
  int b = *(&a);
  int* p = &a;
  int* q = &(*p);
}
"
 (* LATER: replace [dRoot] with [] above *)


let _ = Run.script_cpp (fun _ ->
  (* !! Variable_basic.simpl_deref [dRoot]; --uncomment to simplify all *)

  !! Variable_basic.simpl_deref [cRead ~addr:[cVar "b"] ()];
  !! Variable_basic.simpl_deref [cRead ~addr:[cVar "a"] ()];


  Trace.alternative ( fun () ->
    !! Variable_basic.simpl_deref ~indepth:true [];
    !! ();
  )

)

(*

  New convention: all functions named with "simpl_"  take an ~indepth whose default value in false.
  => beta should be renamed to simpl_beta, and take indepth with true as default
  => infix_ops should follow the same scheme as suggested above for simple_defer

  Motivation: the exact targets of simplification operations are usually numerous and hard to give a target expression for;
  thus in general we want to target a surrounding statement, or even possibly the root.
*)
