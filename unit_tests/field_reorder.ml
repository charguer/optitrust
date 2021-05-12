
open Optitrust


(* Still something to improve *)
let _ = 
    run
    (  fun _ -> 
        set_init_source "field_reorder.cpp";
        fields_reorder [cTypDef "obj"] ~struct_fields:["m";"z"] ~move_before:"x" ();
        fields_reorder [cTypDef "obj"] ~struct_fields:["m"] ~move_after:"x" ();      
        (* LATER: ~move_to_front:true ~move_to_back:false
        module MovePos = struct
          type pos = Before of string | ...
        end
        fields_reorder MovePos.(Before x)
        fields_reorder MovePos.(Front)
        
         *)  
        dump()
        
    )