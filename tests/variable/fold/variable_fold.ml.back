open Optitrust
open Target



let _ = Run.doc_script_cpp (fun _ ->
    !! Variable.fold ~at:[cVarDef "b"] [cVarDef "a"];
  )
"
int main() {
  int x = 1;
  int y = 2;
  const int a = x*y;
  int b = x*y + x*y;
}
"



let _ = Run.script_cpp( fun _ ->
  (* Folding a constant variable *)
  !! Variable.fold ~at:[cVarDef "r1"] [cVarDef "s1"];
  (* Folding a non-constant variable, requires a flag to force it *)
  !! Variable.fold ~nonconst:true [cVarDef "s2"];
  (* Without that flag, an error is issued *)
  !! Trace.failure_expected (fun () ->
        Variable.fold [cVarDef "a"])
)