open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  !! Variable.inline_and_rename [cVarDef "y"];

)

"
int main(){
  const int x = 5;
  const int y = x;
  int z = x + y;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Variable.inline_and_rename [cTopFunDef "test_const_const"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_const"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_const_nonconst"; cVarDef "y"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_nonconst"; cVarDef "y"];

  !! Variable.inline_and_rename [cTopFunDef "test_const_const_vect"; cVarDef "b"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_const_vect"; cVarDef "b"];
  !! Variable.inline_and_rename [cTopFunDef "test_const_nonconst_vect"; cVarDef "b"];
  !! Variable.inline_and_rename [cTopFunDef "test_nonconst_nonconst_vect"; cVarDef "b"];

)
