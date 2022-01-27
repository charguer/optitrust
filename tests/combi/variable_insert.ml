open Optitrust
open Target

(* TODO: here and in other transformations with a typ as argument:
   the typ argument should not be a string, but a typ,
   like the value is a trm; we can apply [typ "int"] to convert a string to a typ. *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable.insert ~typ:"int" ~name:"b" ~value:(lit "2") [tAfter; cVarDef "a"];
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
