open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Arrays_basic.tile "B" ~block_type:"BLOCK" [cTypDef "T"];
)

"
typedef int *T;
int main() {
  T t;
  int a = t[3];
}
"

let _ = Run.script_cpp (fun _ ->

  !! Arrays_basic.tile "B" ~block_type:"U_BLOCK" [cTypDef "U"];
  !! Arrays_basic.tile "B" [cTypDef "T"];
  !! Arrays_basic.tile "B" [cTypDef "V"];

)
