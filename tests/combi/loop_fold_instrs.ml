open Optitrust
open Target

let _ = Run.doc_script_cpp (fun () ->

  !! Loop.fold_instrs ~index:"k" [cWriteVar "a"];

)

"
int main (){
  int a = 0;
  a += 0;
  a += 1;
}
"

let _ = Run.script_cpp (fun _ ->

  (* NOTE: nbMulti is added by default *)
  !! Loop.fold_instrs  ~index:"k" [cWriteVar "a"];
  !! Loop.fold_instrs  ~index:"k" [cCellWrite ~base:[cVar "values"] ~index:[] ()];

)
