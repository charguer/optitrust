open ScriptTools


let _ = 
  run 
  ( fun _ -> 
    set_init_source"inline_seq.cpp";
    (** Does not work correctly *)
    inline_seq  ~seq_path:[[cVarDef ~name:"x" ()] >>! []] ();
    
    dump()
  )