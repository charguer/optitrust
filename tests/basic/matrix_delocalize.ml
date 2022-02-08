open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize (*~init_zero:true*) ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cLabelBody "mark"];

  (* TODO: init_zero as Trace.alternative *)

)


(*

for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        int *sum = new int(0);
        for (int i0 = 0; i0 < N0; i0++) {
          set(REMOVE_get(sum),
              get(sum) + ADDGET_array_access((get(x)), (MINDEX4(N0, N1, N2, N3, i0, i1,
                                                         i2, i3))));
        }
        set(REMOVE_get(array_access((get(a)), (MINDEX3(N1, N2, N3, i1, i2, i3)))),
            get(sum));
      }
    }
  }

  *)