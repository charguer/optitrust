open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Arrays_basic.to_variables ["ta";"tb";"tc"] [cVarDef "t"];
  )
"
typedef int *T;

int main() {
  int t[3];
  int x = t[1];
}
"

(* LATER: make sure that the combi version performs set_explicit on the fly.
    int t[3] = { 1, 2, 3 };
*)

let _ = Run.script_cpp ~parser:Parsers.Clang (fun () ->
  !! Arrays_basic.to_variables ["ua";"ub"] [cVarDef "u"];
  !! Arrays_basic.to_variables ["va";"vb"] [cVarDef "v"];
)

 (* LATER: should support patterns, such as
    Arrays_basic.to_variables (fun base i -> Printf.sprintf "%s%d" base i)
  *)

