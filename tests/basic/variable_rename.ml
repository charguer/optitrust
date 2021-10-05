open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  !! Variable_basic.(rename (AddSuffix "2")) [cTopFunDef "main"; dBody];
  !! Variable_basic.rename (Variable.ByList [("y","y1");("z","z1")]) [cFunDef "f"; dBody];
  !! Variable_basic.(rename (AddSuffix "2")) [cTopFunDef "main"; dBody];
)


(* DONE:  try this

in Variable_core:

module Rename = struct
  type t = AddSuffix of string | ByList of (string * string) list
end

  in Variable and Variable_basic:

   include Variable_core.Rename

then in current file, try both

    Variable_basic.rename (Variable.AddSuffix "2") [cTopFunDef "main"; dBody];

    Variable_basic.(rename (AddSuffix "2")) [cTopFunDef "main"; dBody];
*)

