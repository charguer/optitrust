open Optitrust
open Prelude

let _ = Run.doc_script_cpp (fun _ ->

  !! Accesses_basic.scale ~factor:(trm_double 5.0) [cReadOrWrite ~addr:[cVar "x"] ()]

)

"
int main() {
  int x = 2;
  int y = x;
  x = y;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale ~factor:(trm_double 5.0) [cCellReadOrWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];

)
