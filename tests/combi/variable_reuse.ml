open Optitrust
open Target

(* TODO: perhaps we could make (var "x") a non-named argument? *)
(* TODO: there is an issue with the implementatin of reuse.
   On the example below, the [int y = 2] should be first detached as
   [int y; y = 2]. And then the reuse is invoked on [int y],
   with the effect of removing the declaration, and renaming [y] to [x]. *)

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable.reuse ~space:(var "x") [cVarDef "y"];
  )
"
int main() {
  int x = 0;
  x = x + 1;
  int y = 2;
  y = y + 3;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable.reuse ~space:(var "x") [cVarDef "y"];
)
