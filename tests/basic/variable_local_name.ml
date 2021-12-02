open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.local_name "x" ~into:"y" [cLabel "sec"];
  )
"
int main() {
  int x = 0;
sec:{
  x = x + 1;
  x = x + 2;
}
  int r = x;
}
"
let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.local_name ~mark:"mymark" "a"  ~into:"x" [cFor "i"];
)
