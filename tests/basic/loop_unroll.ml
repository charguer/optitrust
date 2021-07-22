open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Loop.unroll [cFor "i"];
)

(* [unroll_basic] version expects
    for (int i = a; i < a + 8; i++){
   where 8 is a literal (not even a constant def) *)





(* TODO:
   in the combi level, [unroll] is matching the code

       for (int i = a; i < a + N; i++) {

    if N is a variable -> try to call inline_var on this target
    then
    if N is not a literal -> fail
    then
    call the basic unroll


    [unroll_and_shuffle] which does unroll, then [shuffle]

    [shuffle] is a stand alone transformation (see notes)
*)
