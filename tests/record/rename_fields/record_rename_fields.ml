open Optitrust
open Target

let _ = Flags.use_member_functions ()

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.rename_fields (fun x -> "rel_" ^ x) [cTypDef "vect"];
  !! Record_basic.(rename_fields Rename.(only_for "pos" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
  !! Record_basic.(rename_fields Rename.(only_for "speed" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];

  !! Record_basic.rename_fields (fun x -> "new_" ^ x) [cTypDef "TestFieldRename"];

)
