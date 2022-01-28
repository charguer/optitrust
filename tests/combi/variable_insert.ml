open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable.insert ~typ:(atyp "int") ~name:"b" ~value:(lit "2") [tAfter; cVarDef "a"];
  )
"
int main() {
  int a = 1;
  int c = 3;
}
"


let _ = Run.script_cpp (fun _ ->

  !! Variable.insert ~const:true ~name:"a" ~value:(lit "300") [ tAfter; cTypDef "vect"];
  !! Variable.insert ~reparse:true ~name:"b" ~value:(lit "500") [ tAfter; cTypDef "vect"];

)
