open Optitrust
open Target

(* TODO: there is a missing List.rev on the processing of definitions *)
(* TODO: the arguments of ~defs should be a list of  typ*string*trm *)
let _ = Run.doc_script_cpp (fun _ ->
  !! Variable.insert_list  ~defs:[("int","c","3"); ("int","d","4")] [tAfter; cVarDef "b"]
  )
"
int main() {
  int a = 1;
  int b = 2;
}
"


let _ = Run.script_cpp (fun _ ->

    !! Variable.insert_list  ~defs:[("const int","x","1");("const int","y","x/2");("const int","z","y/x")] [tBefore; cFunDef "main"]

)