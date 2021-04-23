open Optitrust


let _ = 
  run
    ( fun () -> 
      set_init_source "pic_demo.cpp";

      let main = [cFun "main"] in
      
      fun_inline [cFun "vect_mul"] ~at:[cStr "pos2 ="] ~res:"accel";      
      fun_inline [cFun "vect_add"] ~at:main;
      var_inline [cVar "accel"] ~at:main;
      var_inline [cFun "main", cVar "p"];

      struct_assign_explicit ~typ:[cTyp "particle"];
      struct_assign_explicit ~typ:[cTyp "vect"];
      struct_assign_explicit ~typ:[cTyp "items"];
      
      struct_inline_field ~typ:[cTyp "particle"] ["pos";"speed"];
      struct_inline_field ~typ:[cTyp "bag"] ["items"];
      
      dump()
    )
