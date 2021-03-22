open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_explicit_assigment.cpp";
        
        (* TODO with arthur: see how to use cList to set up a constraint on a list of arguments *)
        make_explicit_record_assignment [cApp ~name:"overloaded=" ~args:[cVar ~name:"d" ()] ~validate:(function [true;_] -> true | _ -> false) ()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cStr "d = f()"] ~struct_name:"vect";
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        
        make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
        dump()
    )
