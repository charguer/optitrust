open Optitrust
open Target
open Syntax

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable.insert_list  ~defs:[("int","c",lit "3"); ("int","d",lit "4")] [tAfter; cVarDef "b"]
  )
"
int main() {
  int a = 1;
  int b = 2;
}
"


let _ = Run.script_cpp (fun _ ->

    !! Variable.insert_list  ~defs:[("const int","x", lit "1");("const int","y",expr "x/2");("const int","z",expr "y/x")] [tBefore; cFunDef "main"]

)