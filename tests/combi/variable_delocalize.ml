open Optitrust
open Target
open Prelude

let _ = Run.doc_script_cpp (fun () ->
  let a = find_var_in_current_ast "a" in
  !! Variable.delocalize a ~into:"x" ~index:"k" ~mark:"A" ~array_size:(var "N") ~ops:(Local_arith (Lit_int 0, Binop_add) ) [cFor "i"];

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

  let a = find_var_in_current_ast "a" in
  !! Variable.delocalize a ~into:"x" ~index:"k" ~mark:"A" ~array_size:(var "N") ~ops:(Local_arith (Lit_int 0, Binop_add) ) [cFor "i"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name a ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:(var "N") ~ops:(Local_arith (Lit_int 0, Binop_add)) [cMark "section_of_interest"] ;
    !! ());
  let ops = Local_obj (
    name_to_var "clean",
    name_to_var "transfer",
    name_to_var ""
  ) in
  !! Variable.delocalize a ~into:"y" ~index:"k" ~mark:"B" ~array_size:(var "N") ~ops [cFor "j"];
  !! Trace.alternative (fun () ->
    !! Variable.local_name  a ~into:"x" ~mark:"section_of_interest" [cFor "i"];
    !! Variable_basic.delocalize  ~array_size:(var "N") ~ops [cMark "section_of_interest"] ;
    !! ());

)
