open ScriptTools

let _ = 
  run 
    (fun () ->
      set_init_source "test_pic_0.cpp";
      
      let speed_variable = [cVarDef ~name:"speed2" ()] in 
      
      insert_and_fold ~insert_before:speed_variable ~name:"v1" ~value:"p.speed" ();
    
      inline_decl ~delete_decl:true  ~fun_result:"res"~decl_path:[cFun ~name:"vect_mul" ()] ();
      
      dump ()
      )