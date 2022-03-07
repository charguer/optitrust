open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.bind "b" [cFun "f"];
  )
"
int f(int x);
int g(int x);

int main() {
  int a = 1;
  int r = g(f(a + 3));
}
"

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->
   
  !! Variable_basic.bind "a" ~const:true [cFunDef "test"; cReturn; cArrayInit];
  !! Variable_basic.bind "b" [cVarDef "arr"; cArrayInit];
  !! Variable_basic.bind "z" ~is_ptr:true [cFun "pointer_arg"; dArg 0];
)
 