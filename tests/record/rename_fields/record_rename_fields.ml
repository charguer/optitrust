open Optitrust
open Target

let _ = Flags.dont_serialize := true (* because member functions *)
let _ = Flags.ignore_serialized := true (* because member functions *)

let _ = Run.script_cpp (fun _ ->

  !! Record_basic.rename_fields (fun x -> "rel_" ^ x) [cTypDef "vect"];
  !! Record_basic.(rename_fields Rename.(only_for "pos" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];
  !! Record_basic.(rename_fields Rename.(only_for "speed" (fun x ->  "rel_" ^ x))) [cTypDef "obj"];

  !! Record_basic.rename_fields (fun x -> "new_" ^ x) [cTypDef "TestFieldRename"];

)
