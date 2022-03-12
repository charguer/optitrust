open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

   
   !! Matrix.delocalize "a" ~into:"x" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cFor "i"];
    
   !! Trace.alternative (fun () ->
        !! Matrix.delocalize "a" ~into:"x" ~last:true  ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cFor "i"];
        !!());

  !! Trace.alternative (fun () ->
        !! Matrix.delocalize "a" ~use:(Some (expr"k")) ~into:"x" ~init_zero:true ~acc_in_place:false  ~acc:"sum" ~dim:(var "N0") ~index:"i0" ~ops:(Local_arith (Lit_int 0, Binop_add)) [cFor "i"];
        !!());

)

