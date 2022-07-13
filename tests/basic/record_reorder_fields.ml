
open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->
    !! Record_basic.reorder_fields (Reorder_all ["m"; "x"; "y"]) [cTypDef "obj"];
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
  
  !! Record_basic.reorder_fields (Move_before ("x", ["m";"z"])) [cTypDef "obj"];
  !! Record_basic.reorder_fields (Move_after ("y", ["z"])) [cTypDef "obj"];
  !! Record_basic.reorder_fields (Move_after ("z", ["y";"m"])) [cTypDef "obj"];
  !! Record_basic.reorder_fields (Reorder_all ["x";"y";"z"; "m"]) [cTypDef "obj"];

  !! Record_basic.reorder_fields (Move_before ("x", ["f";"g"])) [cTypDef "OBJ"];
  !! Record_basic.reorder_fields (Reorder_all ["x";"f";"g"]) [cTypDef "OBJ"];
  
)
