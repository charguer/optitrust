open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->
    !! Variable.insert_and_fold ~typ:(ty "const int") ~name:"a" ~value:(expr "x*y") [tBefore; cVarDef "r"];
  )
"
int main() {
  int x, y;
  int r = (x * y) * (x * y);
}
"


let _ = Run.script_cpp (fun _ ->
  !! Variable.insert_and_fold ~name:"s1" ~typ:(ty "const int") ~value:(expr "x*y") [tAfter;cVarDef "y"];
  !! Variable.insert_and_fold ~name:"s2" ~typ:(ty "const int") ~value:(expr "y*x") [tAfter;cVarDef "r1"];
  !! Variable.insert_and_fold ~name:"s3" ~typ:(ty "const int") ~value:(expr "f(2,2)") [tAfter;cVarDef "r2"];

)
