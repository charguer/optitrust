open Optitrust


let _ = 
  run
    ( fun () -> 
      set_init_source "pic_demo_stable.cpp";
      
      detach_expression [cVarDef ~name:"speed2"()] ~keep_label:false; 
      (*2*) inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"vect_add" ()] ~inline_at:[[cTopFun ~name:"main"()]] ~fun_args:["mv1";"mv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"mv1" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"mv2" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      
      set_repeat_io false; 
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      (*3*) make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar ~name:"speed2"()]  ()]; 
      set_repeat_io true; 

      detach_expression [cVarDef ~name:"pos2"()] ~keep_label:false;
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"vect_add" ()] ~inline_at:[[cTopFun ~name:"main"()]] ~fun_args:["nv1";"nv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"nv1" ()] ~inline_at:[[cTopFun ~name:"main"()]]();

      set_repeat_io false; 
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar ~name:"pos2"()]  ()]; 
      set_repeat_io true;
      const_non_const [cVarDef ~name:"nv2"()];
      detach_expression [cVarDef ~name:"nv2"()] ~keep_label:false;
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"vect_mul" ()] ~inline_at:[[cTopFun ~name:"main"()]] ~fun_args:["pv1";"pv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"pv1" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"pv2" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      
      set_repeat_io false; 
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      inline_record_access ~field:"x" ~var:"nv2" ();     
      inline_record_access ~field:"y" ~var:"nv2" ();   
      inline_record_access ~field:"z" ~var:"nv2" ();
      undetach_expression [cVarDef ~name:"nv2"()];
      remove_decl ~decl_path:[cVarDef ~name:"nv2"()] ();
      set_repeat_io true;

      (*1*) inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"p"()] ~inline_at:[[cTopFun ~name:"main"()]]();


      make_explicit_record_assignment ~struct_name:"particle" [cFun ~name:"bag_push"(); cSet ~rhs:[cVar ~name:"p"()]()];

      make_explicit_record_assignment ~struct_name:"vect" [cFun ~name:"bag_push"(); cStr ~regexp:true ".*= p\\.pos"];
      make_explicit_record_assignment ~struct_name:"vect" [cFun ~name:"bag_push"(); cStr ~regexp:true ".*= p\\.speed"];
      
      set_repeat_io false;
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"bag_push" ()] ~fun_args:["mb";"mp"] ();
      const_non_const [cVarDef ~name:"mp"()];
      const_non_const [cVarDef ~name:"mb"()];
      set_repeat_io false;

      set_repeat_io true;
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"p2" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      inline_decl ~delete_decl:true ~decl_path:[cFun ~name:"main"(); cVarDef ~name:"mp" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      set_repeat_io false; 
      
      (*4*) inline_struct ~struct_name:"particle" ~struct_fields:["pos";"speed"] ();
      inline_struct ~struct_name:"bag" ~struct_fields:["items"] ();
     
      make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar ~name:"pos2"()]  ()]; 

      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"nv2" ()] ~inline_at:[[cTopFun ~name:"main"()]]();

      make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar ~name:"nv2"()]  ()]; 
      show_ast [cVarDef ~name:"p"()] ;
      
      inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"vect_mul" ()] ~inline_at:[[cTopFun ~name:"main"()]]~fun_args:["rv1";"rv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"v1"()] ~inline_at:[[cTopFun ~name:"main"()]]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"v"() ] ~inline_at:[[cTopFun ~name:"main"()]] ();
      
      inline_record_access ~field:"x" ~var:"v2" ();     
      inline_record_access ~field:"y" ~var:"v2" ();   
      inline_record_access ~field:"z" ~var:"v2" ();

      detach_expression [cVarDef ~name:"pos2"()] ~keep_label:false; 
      inline_decl ~delete_decl:true ~decl_path:[cTopFun ~name:"vect_add" ()] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"res" ()] ();
      make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar ~name:"pos2"()]  ()]; 
      inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"v1"()] ~inline_at:[[cTopFun ~name:"main"()]]();

      dump()
    )
