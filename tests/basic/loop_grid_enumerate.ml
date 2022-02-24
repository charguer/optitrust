open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.grid_enumerate [("x", "3"); ("y", "4")] [cFor "c"];
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

let _ = Run.script_cpp ~parser:Parsers. (fun _ ->
  !! Loop_basic.grid_enumerate [("x", "X"); ("y", "Y"); ("z", "Z")] [cFor "idCell"];
)
