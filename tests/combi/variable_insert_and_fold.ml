open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Variable.insert_and_fold ~name:"a" ~typ:"int" ~value:(expr "x*y") [tBefore; cVarDef "r"];
  )
"
int main() {
  int x, y;
  int r = (x * y) * (x * y);
}
"

let _ = Run.script_cpp (fun _ ->
  !! Variable.insert_and_fold ~name:"s1" ~typ:"int" ~value:(expr "x*y") [tAfter;cVarDef "y"];
  !! Variable.insert_and_fold ~name:"s2" ~typ:"int" ~value:(expr "y*x") [tAfter;cVarDef "r1"];
  !! Variable.insert_and_fold ~name:"s3" ~typ:"int" ~value:(expr "f(2,2)") [tAfter;cVarDef "r2"];

)