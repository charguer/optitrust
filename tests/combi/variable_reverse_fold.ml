open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Variable.reverse_fold [cVarDef "y"];
  (* TODO:
      we should be careful to write in the documentation that this transfo is only
      legitimate if the variable x in the "int y = x" that we eliminate does not
      occur anywere after the instruction "int y = x".

      TODO: I wonder if this definition shouldn't be implemented and described simply
      as "inline of def of y" followed with "rename x into y".
      If so, the right name for this transfo would be  variable.inline_and_rename.
   *)

)