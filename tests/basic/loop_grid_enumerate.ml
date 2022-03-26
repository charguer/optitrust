open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.grid_enumerate [("x", lit "3"); ("y", lit "4")] [cFor "c"];
  )
"
int main() {
  for (int c = 0; c < 3*4; c++) {
  }
}
"
(* LATER/ in the combi version, if the original loop has the form "c < X*Y*Z",
   then we could automatically use the items from the product to guide the loops;
   in this case, the user only need to give ~indices:["cx","cy","cz"] *)

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.grid_enumerate [("x", expr "X"); ("y", expr "Y"); ("z", expr "Z")] [cFor "idCell"];
)
