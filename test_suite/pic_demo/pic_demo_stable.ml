open Optitrust


let _ =
  run
    ( fun () ->
      set_init_source "pic_demo_stable.cpp";

      detach_expression [cVarDef "speed2"] ~keep_label:false;
      (*1*) inline_decl ~delete_decl:false ~decl_path:[cTopFun"vect_add"] ~inline_at:[[cTopFun"main"]] ~fun_args:["mv1";"mv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "mv1" ] ~inline_at:[[cTopFun"main"]]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "mv2" ] ~inline_at:[[cTopFun"main"]]();

      set_repeat_io false;
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "res" ] ~inline_at:[[cTopFun"main"]]();
      (*2*) make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar "speed2"]  ()];
      set_repeat_io true;

      detach_expression [cVarDef "pos2"()] ~keep_label:false;
      inline_decl ~delete_decl:false ~decl_path:[cTopFun"vect_add"] ~inline_at:[[cTopFun"main"]] ~fun_args:["nv1";"nv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "nv1"] ~inline_at:[[cTopFun"main"]]();

      set_repeat_io false;
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "res"] ~inline_at:[[cTopFun"main"]]();
      make_explicit_record_assignment ~struct_name:"vect" [cSet ~lhs:[cVar "pos2"]  ()];
      set_repeat_io true;
      const_non_const [cVarDef "nv2"];
      detach_expression [cVarDef "nv2"] ~keep_label:false;
      inline_decl ~delete_decl:false ~decl_path:[cTopFun"vect_mul" ] ~inline_at:[[cTopFun"main"]] ~fun_args:["pv1";"pv2"] ();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "pv1" ] ~inline_at:[[cTopFun"main"]]();
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "pv2" ] ~inline_at:[[cTopFun"main"]]();

      set_repeat_io false;
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "res" ] ~inline_at:[[cTopFun"main"]]();
      inline_record_access ~field:"x" ~var:"nv2" ();
      inline_record_access ~field:"y" ~var:"nv2" ();
      inline_record_access ~field:"z" ~var:"nv2" ();
      undetach_expression [cVarDef "nv2"];
      remove_decl ~decl_path:[cVarDef "nv2"] ();
      set_repeat_io true;

      (*3*) inline_decl ~delete_decl:true ~decl_path:[cVarDef "p"] ~inline_at:[[cTopFun"main"]]();


      make_explicit_record_assignment ~struct_name:"particle" [cFunDef "bag_push"; cSet ~rhs:[cVar ~name:"p"]()];

      make_explicit_record_assignment ~struct_name:"vect" [cFunDef "bag_push"; cStr ~regexp:true ".*= p\\.pos"];
      make_explicit_record_assignment ~struct_name:"vect" [cFunDef "bag_push"; cStr ~regexp:true ".*= p\\.speed"];

      set_repeat_io false;
      inline_decl ~delete_decl:false ~decl_path:[cTopFun "bag_push"] ~fun_args:["mb";"mp"] ();
      const_non_const [cVarDef "mp"];
      const_non_const [cVarDef "mb"()];
      set_repeat_io false;

      set_repeat_io true;
      inline_decl ~delete_decl:true ~decl_path:[cVarDef "p2" ] ~inline_at:[[cTopFun"main"]]();
      inline_decl ~delete_decl:true ~decl_path:[cFunDef ~name:"main"; cVarDef "mp" ] ~inline_at:[[cTopFun"main"]]();
      set_repeat_io false;

      (*4*) inline_struct ~struct_name:"particle" ~struct_fields:["pos";"speed"] ();
      (*5*) inline_struct ~struct_name:"bag" ~struct_fields:["items"] ();

      dump()
    )
