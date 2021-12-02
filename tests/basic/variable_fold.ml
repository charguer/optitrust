open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.fold  ~at:[cVarDef "r"] [cVarDef "a" ];
  )
"
int main() {
  int x, y;
  int a = (x * y);
  int r = (x * y) * (x * y);
}
"

let _ = Run.script_cpp (fun _ ->
  !! Variable_basic.fold ~at:[cVarDef "r1"] [cVarDef "s1" ];
)
