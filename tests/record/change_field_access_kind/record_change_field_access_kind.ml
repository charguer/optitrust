open Optitrust
open Target
open Prelude


let _ = Run.script_cpp (fun _ ->

  (* changing access kind of a struct member *)
  !! Record_basic.change_field_access_kind (Access_public) ~field:"x" [cTypDef "test_struct"];
  !! Record_basic.change_field_access_kind (Access_public) ~field:"y" [cTypDef "test_struct"];
  !! Record_basic.change_field_access_kind (Access_private) ~field:"z" [cTypDef "test_struct"];

  (* changing access kind of a class member *)
  !! Record_basic.change_field_access_kind (Access_public) ~field:"x" [cTypDef "test_class"];
  !! Record_basic.change_field_access_kind (Access_private) ~field:"z" [cTypDef "test_class"];
  !! Record_basic.change_field_access_kind (Access_private) ~field:"f" [cTypDef "test_class"];

)