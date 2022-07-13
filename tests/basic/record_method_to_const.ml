open Optitrust
open Target 


let _ = Run.script_cpp (fun _ ->

  
  (* Converting method get1 of class Conset_method_test to a const method. *)
  !! Record_basic.method_to_const ~method_name:"get1" [cTypDef "Method_test"];

  (* Converting method get1 of class Conset_method_test to a const method. *)
  !! Record_basic.method_to_const ~method_name:"get2" [cTypDef "Method_test"];

  
  (* Converting all methods to const methods. *)
  !! Trace.alternative (fun _ -> 
    !! Record_basic.method_to_const [cTypDef "Method_test"];
    !! ();) 

)
