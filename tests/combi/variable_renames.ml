open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.renames(ByList [("x","y")]) [cFunDef "main"; dBody];
  )
"
int main() {
  int a = 3;
  int x = a;
  int r = x;
}
" *)

let _ = Run.script_cpp (fun _ ->

  !! Variable.(renames(AddSuffix "2")) [cTopFunDef "main"; dBody];
  !! Variable.renames(ByList [("y","y1");("z","z1")]) [cFunDef "f"; dBody];
  !! Variable.(renames(AddSuffix "2")) [cTopFunDef "main"; dBody];
)
