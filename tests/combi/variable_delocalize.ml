open Optitrust
open Target
open Syntax

let _ = Run.doc_script_cpp (fun () ->

  !! Variable.delocalize "a" ~into:"x" ~index:"k" ~mark:"A" ~array_size:"N" ~ops:(Local_arith (Lit_int 0, Binop_add) ) [cFor "i"];

)
"
#include \"../../include/optitrust.h\"
typedef int T;

int main(){
  const int N = 2;
  T a;
  for (int i = 0; i < N; i++){
     a++;
  }
  for (int j = 0; j < N; j++){
     a++;
  }
  return 0;
}
"



let _ =  Run.script_cpp ( fun () ->

  !! Variable.delocalize "a" ~into:"x" ~index:"k" ~mark:"A" ~array_size:"N" ~ops:(Local_arith (Lit_int 0, Binop_add) ) [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name "a" ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:"N" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! ());
  !! Variable.delocalize "a" ~into:"y" ~index:"k" ~mark:"B" ~array_size:"N" ~ops:(Local_obj ("clean", "transfer", "")) [cFor "j"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  "a" ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:"N" ~ops:(Local_obj ("clean", "transfer", "")) [cMark "section_of_interest"] ;
    !! ());

)
