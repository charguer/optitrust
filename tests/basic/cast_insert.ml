open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Cast_basic.insert Ast.(typ_double ()) [cVar "a"];
  )
"
int main() {
  float a = 1.;
  double b = a;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Cast_basic.insert Ast.(typ_double ()) [cVar "a"];
  !! Cast_basic.insert Ast.(typ_float ()) [cVar "b"];

)