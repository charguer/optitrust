open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.to_unit_steps ~index:"s" [cFor "i"];
  )
"
int main() {
  for (int i = 4; (i < 10); i += 2) {
    int x = i;
  }
}
"
(* TODO: simplify the expression when the start is zero, there is no need to subtract zero;
   in other cases, even if we have constant values, it's fine to leave the subtraction (eg. 10-4). *)


let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.to_unit_steps ~index:"j" [cFor "i"];
  (* if not provided, the new index name is generated from the original one *)
  !! Trace.alternative (fun () ->
    !! Loop_basic.to_unit_steps [cFor "i"];
    !!(); )
  )

(* LATER: we will one day need the transformation

  for (int i = 0; i < n; i++) {
    int x = 2*i;
  }

  to

  int x = 0;
  for (i..) {
    x += 2;


*)
