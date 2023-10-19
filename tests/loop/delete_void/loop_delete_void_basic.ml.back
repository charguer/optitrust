open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.delete_void [cFor "i"];
)

"
int main (){
  for (int i = 0; i < 10; i++) {
  }
}
"

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.delete_void [cFor "i"];
  !! Loop_basic.delete_void [cFor "i3"];
  !! Loop_basic.delete_void [cFor "i2"];
  !! Trace.failure_expected (fun () ->
    Loop_basic.delete_void [cFor "j"]
  );
  !! Trace.failure_expected (fun () ->
    Loop_basic.delete_void [cFor "j2"]
  );
)
