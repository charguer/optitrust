open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  (* TODO: foo ---> if (cond) then foo else foo
     where foo is the target instruction
     the implementation does not need to know about surrounding sequence *)
  !! Generic.arbitrary_if ~single_branch:true "x > 0" [sInstr "x = 5"];
  !! Generic.arbitrary_if  "y > 0" [sInstr "y = 2"];
  (* TODO: an example where you call sub to group 2 instructions in a seq
     and then do arbitrary_if on that sequence, meaning that both branches
     now contain the 2 instructions *)
)
