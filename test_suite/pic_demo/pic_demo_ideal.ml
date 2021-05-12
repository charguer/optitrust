open Optitrust


let _ = 
  run
    ( fun () -> 
      set_init_source "pic_demo_stable.cpp";

      fun_inline [cFunDef "vect_mul"] ~at:[cStr "pos2 ="] ~res:"accel";      
      fun_inline [cFunDef "vect_add"] ~at:[cFunDef "main"];
      var_inline [cVar "accel"] ~at:[cFunDef "main"];
      var_inline [cFunDef "main", cVar "p"];

      struct_assign_explicit ~typ:[cTyp "particle"];
      struct_assign_explicit ~typ:[cTyp "vect"];
      struct_assign_explicit ~typ:[cTyp "items"];
      
      struct_inline_field ~typ:[cTyp "particle"] ["pos";"speed"];
      struct_inline_field ~typ:[cTyp "bag"] ["items"];
      
      dump()
    )
