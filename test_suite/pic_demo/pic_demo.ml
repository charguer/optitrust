open Optitrust


let _ = 
  run
    ( fun () ->
      set_init_source "pic_demo.cpp";
      (* show_path [cVarDef ~name:"nbSteps"()]; *)
      (*show_path [cTopFun ~name:"vect_mul" ()];*)
      (* show_ast [cVarDef ~name:"cdv3"()]; *)
      detach_expression [cVarDef ~name:"v3"()] ~keep_label:false; 
      (* inline_decl ~delete_decl:false ~decl_path:[cTopFun ~name:"v_add" ()] (); *)
      (* show_path [cApp ~args:[cVar ~strict:true ~name:"p2" ()] ~validate:(List.mem true) ()] ~debug_ast:true; *)
      inline_decl ~delete_decl:true~decl_path:[cTopFun ~name:"bag_push" ()] ~inline_at:[[cTopFun ~name:"main"()]]();
      (* TODO: Fix the error with variable bound *)
      (* show_path [cVarDef ~name:"v3"()] ~debug_ast:true;  *)
      make_explicit_record_assignment ~struct_name:"particle" [cApp ~args:[cVar ~strict:true ~name:"p_0" ()] ~validate:(List.mem true) ()];
      (* TODO: if struct_name is not provided, it is infered from the type of the LHS or RHS of the assignement *)
      (* LATER: also genertate a show_path on the path involved in the last transformation. *)
(* make_explicit_record_assignment *)
      dump()
    )