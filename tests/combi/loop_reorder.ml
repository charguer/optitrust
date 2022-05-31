
open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  
  !! Loop.reorder ~order:["c";"b";"a"] [cFor "a"];

)

"
int main() {
  for (int a = 0; a < 4; a++) {
    for (int b = 0; b < 5; b++) {
      for (int c = 0; c < 6; c++) {
      }
    }
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Loop.reorder ~order:["d";"c";"b"] [cFor "b"];

  !! Trace.alternative (fun () ->
    !! Loop.reorder ~order:["b";"d";"e";"a";"c" ] [cFor "a"];
    !!(););

  !! Trace.alternative (fun () ->
    !! Loop.reorder ~order:["e";"d"] [cFor "d"];
    !! Loop.reorder ~order:["a"] [cFor "a"]; (* identity *)
    !!(););

  !! Trace.alternative (fun () ->
    !! Tools.failure_expected (fun () ->
       Loop.reorder ~order:["e"] [cFor "a"];);
    !! Tools.failure_expected (fun () ->
       Loop.reorder ~order:["e"; "f"] [cFor "e"];)
    );

)
