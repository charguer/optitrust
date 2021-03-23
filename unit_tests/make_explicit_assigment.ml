open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"make_explicit_assigment.cpp";
        
        (* TODO with arthur: see how to use cList to set up a constraint on a list of arguments *)
        (* For initializations which include declarations the following syntax is used *)
        make_explicit_record_assignment [cVarDef ~name:"b"()] ~struct_name:"vect"; 
        (*An alternative to that is the following one 
          1) First detach the expression by using : detach_expression [cVardef ~name:"b"()]
          2) Then make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
          However this is done automatically from make_explicit_record_assignment transformation
        *)
        (* For expression which are just assignments *)
        make_explicit_record_assignment [cStr "d = p"] ~struct_name:"vect";
        (* An alternative to that is the folowing one:
          make_explicit_record_assignment [cApp ~name:"overloaded=" ~args:[cVar ~name:"d" ()] ~validate:(function [true;_] -> true | _ -> false) ()] ~struct_name:"vect";
        *)
        
        
        (* 
          This returns an error since function calls are not supported 
        *)
        make_explicit_record_assignment [cStr "d = f()"] ~struct_name:"vect"; 
        
        
        
        dump()
    )
