open Optitrust
open Target
open Prelude



let _ = Run.doc_script_cpp (fun _ ->

  !! Align_basic.def (lit "16") [nbMulti; cVarDef ""];

)

"
const double coeffs_x[2]  = {  1.,  1.};
const double coeffs_y[2]  = {  1.,  1.};

double* a;

int main(){}
"

let _ = Run.script_cpp (fun () ->

  !! Align_basic.def (lit "16") [nbMulti; cVarDef ~regexp:true "coeff.*"];
  !! Align_basic.def (lit "16") [nbMulti; cVarDef "a"];

)
