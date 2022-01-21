open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->
  !! Instr_basic.replace (lit "20") [sExpr "a * 4"];
  )
"
int main() {
  int a = 5;
  int x = a * 4;
}
"

let _ = Run.script_cpp (fun _ ->
  (* replace multiple occurrences of variable "a" with variable "b" *)
  !! Instr_basic.replace (var "b") [nbMulti; cVar "a"];
  (* replace the definition of "x" with a definition of "y" --somewhat arbitrary *)
  !! Instr_basic.replace (stmt "int y = 5;") [cVarDef "x"];
  (* replace the definition of "c" with a call to "f" --somewhat arbitrary *)
  !! Instr_basic.replace (stmt "f(5);") [cVarDef "c"];
  (* replace, in the definition of "d", the first argument to f2, with the constant 3 *)
  !! Instr_basic.replace (lit "3") [cVarDef "d"; cFun "f2"; dArg 0];
  (* replace, in the definition of "e", all constants "2" with the constant "1" *)
  !! Instr_basic.replace (lit "1") [nbMulti; cVarDef "e"; cFun "f2"; cInt 2 ];
  (* replace the function "f2" with another one *)
  !! Instr_basic.replace (stmt "int f2(int a, int b, int c) { return a ; }") [cFunDef "f2" ];
)