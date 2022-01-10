open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Sequence_basic.partition ~braces:true [2;3] [cFunDef "main"; dBody]; 
  )
"
int main() {
  int a = 0;
  int b = 1;
  int c = 2;
  int d = 3;
  int e = 4;
}
"

let _ = Run.script_cpp (fun _ ->
  (* LATER: ARTHUR why is this giving 2 results?
     show [ cFunDef "main"; cSeq(); cSeq()];
   *)
  let tg = [cSeq ~args_pred:(Target.target_list_one_st [cVarDef "a"]) ()] in (* LATER: simplify *)
  !! Sequence_basic.partition ~braces:true [2;3;2] tg;


  (* Error message if the sum of blocks does not correspond to the number trms in the sequence*)
  !! Tools.failure_expected (fun _ ->
    Sequence_basic.partition ~braces:true [2;3] tg;);

  !! Trace.alternative (fun () ->
     Sequence_basic.partition ~braces:true [1;2;1;3] tg;
     !!());

)

(* LATER: sequence.partition might be implementable using sequence.intro
   (maybe less efficiently, but with less code) *)