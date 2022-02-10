open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
    !! Flow_basic.insert_if ~cond:(expr "x > 0") [sInstr "x++"];
  )
"
int main() {
  int x = 0;
  x++;
}
" *)


let _ = Run.script_cpp (fun _ ->
  (* Demo with a single instruction *)
  !! Flow_basic.insert_if ~cond:(expr "x > 0") [cWriteVar "x"];

  (* Demo with a block *)
  !! Sequence_basic.intro ~mark:"foo" 2 [sInstr "b = 4"];
  !! Flow_basic.insert_if ~cond:(expr "x > 0") [cMark "foo"];


  (* Another demo with a block *)
  !! Trace.alternative (fun () ->
      !! Sequence_basic.intro ~mark:"new_block" 2 [sInstr "x = 5"];
      !! Flow_basic.insert_if ~cond:(expr "x > 0") [cMark "new_block"];
      !!();
  )

)
