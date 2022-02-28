open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.unroll [cFor "a"];
  )
"
int main() {
  int r = 10;
  for (int a = 0; a < 5; a++) {
    r += a;
  }
}
"

let _ = Run.script_cpp ~parser:Parsers.Clang (fun _ ->

  (* With partitioning *)
  !! Loop.unroll ~shuffle:true ~blocks:[2;1;2] [cFor "i"];
  !! Loop.unroll  [cFor "j"];

  (* Without partitioning *)
  !! Trace.alternative (fun _ ->
    !! Loop.unroll  [cFor "i"];
    !! (););

  (* Hiding braces *)
  !! Trace.alternative (fun () ->
    !! Loop.unroll ~braces:false [cFor "i"];
    !!())
)

