open Optitrust
open Prelude
let _ = Flags.check_validity := false
let delocalize_sum = Local_arith (Lit_float (typ_f64,0.)  , Binop_add)
let _ = Run.script_cpp (fun _ ->

  !! Loop.fission ~nest_of:2 [cWrite ~lhs:[cVar "A"] (); tBefore];
  !! Omp.parallel [nbMulti; cFor "i"];
  !! Matrix_basic.delocalize ~init_zero:true ~dim:(var_mut "nbThreads") ~index:"j" ~acc_in_place:true ~ops:delocalize_sum  [cFor "j";tAfter] ;
  !! Matrix.delocalize "depositCorners" ~into:"depositThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var_mut "nbThreads") ~index:"idThread" ~acc_in_place:true ~ops:delocalize_sum ~use:(Some (var "idThread"))
      ~labels:["alloc"; ""; "dealloc"] ~alloc_instr ~dealloc_tg:(Some [cTopFunDef ~regexp:true "dealloc.*"; cFor ""])
      [cLabel "core"]
  ;)


