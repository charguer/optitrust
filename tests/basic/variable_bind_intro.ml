open Optitrust
open Target

(* TODO: change name and prototype of this function to match:
      Variable_basic.bind "b" [cFun "f"];
   I don't see why we'd need an optional argument for the name. *)

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.bind_intro  ~fresh_name:"b"  [cFun "f"];
  )
"
int f(int x);
int g(int x);

int main() {
  int a = 1;
  int r = g(f(a + 3));
}
"


let _ = Run.script_cpp (fun _ ->

  !! Variable_basic.bind_intro  ~fresh_name:"a" ~const:true [cFunDef "test"; cReturn;cArrayInit];
  !! Variable_basic.bind_intro  ~fresh_name:"b"  [cVarDef "x"; cArrayInit];
)
