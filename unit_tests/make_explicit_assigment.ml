open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_explicit_assigment.cpp";
        (*make_explicit_record_assigment "vect" *)
        (*
        make_explicit_record_assignment [cSet ~lhs:[cAccesses ~accesses:[cField ~field:"pos"()]  ()] ~rhs:[cVar ~name:"pos"()]()] ~struct_name:"vect";
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect";
        
        *)
        (*
          
          show_path [cApp ~name:"overloaded=" ~args:[cVar ~name:"b" ()] ~validate:(function [true;_] -> true | _ -> false) ()];
        *)
        (* TODO with arthur: see how to use cList to set up a constraint on a list of arguments *)
        
        make_explicit_record_assignment [cApp ~name:"overloaded=" ~args:[cVar ~name:"c" ()] ~validate:(function [true;_] -> true | _ -> false) ()] ~struct_name:"vect";
        
        detach_expression [cVarDef ~name:"b" ()];
        make_explicit_record_assignment [cStr "b = p"] ~struct_name:"vect" ;
        make_explicit_record_assignment [cApp ~name:"overloaded=" ~args:[cVar ~name:"b" ()] ~validate:(function [true;_] -> true | _ -> false) ()] ~struct_name:"vect";
        
        dump()
    )
