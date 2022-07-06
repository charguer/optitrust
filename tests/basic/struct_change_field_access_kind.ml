open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ -> 

  !! Struct_basic.change_field_access_kind (Access_public) [cTypDef "vect"];

)

"
struct vect {
  private:
    int x;
    int y;
}

int main(){}
"

let _ = Run.script_cpp (fun _ -> 

  (* changing access kind of a struct member *)
  !! Struct_basic.change_field_access_kind (Access_public) ~field:"x" [cTypDef "test_struct"];
  !! Struct_basic.change_field_access_kind (Access_public) ~field:"y" [cTypDef "test_struct"];
  !! Struct_basic.change_field_access_kind (Access_private) ~field:"z" [cTypDef "test_struct"];
  
  (* changing access kind of a class member *)
  !! Struct_basic.change_field_access_kind (Access_public) ~field:"x" [cTypDef "test_class"];
  !! Struct_basic.change_field_access_kind (Access_private) ~field:"z" [cTypDef "test_class"];
  !! Struct_basic.change_field_access_kind (Access_private) ~field:"f" [cTypDef "test_class"];

)