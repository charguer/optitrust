
open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.reorder_fields ["m"; "x"; "y"] [cTypDef "obj"];
  )
"
typedef struct {
  int x;
  int y;
  int m;
} obj;

int main() {
  obj o = { 0, 0, 1 };
  return o.x;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Struct_basic.reorder_fields ~move_before:"x" ["m";"z"] [cTypDef "obj"];
  !! Struct_basic.reorder_fields ~move_after:"y" ["z"] [cTypDef "obj"];
  !! Struct_basic.reorder_fields ~move_after:"z" ["y"; "m"] [cTypDef "obj"];
  !! Struct_basic.reorder_fields ["x"; "y"; "z"; "m"] [cTypDef "obj"];
)
