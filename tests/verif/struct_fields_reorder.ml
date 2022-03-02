
open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Struct_basic.fields_reorder ["m"; "x"; "y"] [cTypDef "obj"];
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
  !! Struct_basic.fields_reorder ~move_before:"x" ["m";"z"] [cTypDef "obj"];
  !! Struct_basic.fields_reorder ~move_after:"y" ["z"] [cTypDef "obj"];
  !! Struct_basic.fields_reorder ~move_after:"z" ["y"; "m"] [cTypDef "obj"];
  !! Struct_basic.fields_reorder ["x"; "y"; "z"; "m"] [cTypDef "obj"];
)
