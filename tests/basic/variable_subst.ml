open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->
  
  !! Variable_basic.subst ~subst:"a" ~put:(lit "3") [cVarDef "b"];
  
)

"
int main() {
  int a = (2 + 1);
  int b = a;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.subst ~subst:"y" ~put:(expr "2 + x") [cVarDef "z"];
  !! Variable_basic.subst ~subst:"y" ~put:(Ast.trm_int 5) [cFunDef "main"];

)
