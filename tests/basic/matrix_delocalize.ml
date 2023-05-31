open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~dim:(var "N2") ~index:"i2" ~acc:"sum" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cLabel "mark"];

)

"
#include \"../../include/optitrust.h\"
typedef int T;
int main(){
  int const N1 = 5;
  T* a = (T*) CALLOC1 (N1, sizeof(T));
  mark :{

    T* x = (T*) CALLOC1 (N1, sizeof(T));

    for (int i1 = 0; i1 < N1; i1++){
      x[MINDEX1(N1, i1)] = a[MINDEX1(N1, i1)];
    }

    for (int i = 0; i < 10; i++){
      int t = x[MINDEX1(N1, i)];
      x[MINDEX1(N1, i)];
    }

    for (int i1 = 0; i1 < N1; i1++){
      a[MINDEX1(N1, i1)] = x[MINDEX1(N1, i1)];
    }

    MFREE(x);

  }
  MFREE(a);
  int y = 0;
  return 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cLabel "mark"];

  !! Trace.alternative (fun () ->
    !! Matrix_basic.delocalize ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cLabel "mark"];
    !!();
  )

)
