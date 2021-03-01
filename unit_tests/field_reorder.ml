
open ScriptTools


(* Still something to improve *)
let _ = 
    run
    (  fun _ -> 
        set_init_source"field_reorder.cpp";
        fields_reorder [cType ~name:"obj" ()]  ~struct_fields:["m";"z"] ~move_before:"x" ();
        dump()
        
    )