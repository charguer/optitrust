open Optitrust



let _ =
    run
    ( fun _ ->
        set_init_source"make_explicit_record_assignment.cpp";
        ();
        (* TODO : Implement the reverse of detach_expression *)
        (* TODO with arthur: see how to use cList to set up a constraint on a list of arguments *)
        (* For initializations which include declarations the following syntax is used *)
        make_explicit_record_assignment [[cVarDef "b"]] ~struct_name:"vect";
        (* TODO : infer struct name if easy from LHS *)
        (* make_explicit_record_assignment ~struct_name:"vect" [cApp ~args:[cVar target ~name:"p2" ()] ~validate:(List.mem true) ()]; *)
        make_explicit_record_assignment [cVarDef "p2"] ~struct_name:"vect";
        (* p = { 1, 2}  -->   p.x =1; p.y =2 TODO *)
        (* show_path [cVarDef "e"] ~debug_ast:true;  *)
        (*An alternative to that is the following one
          1) First detach the expression  by using : detach_expression [cVardef "b"]
          2) Then make_explicit_record_assignment [cLabel ~label:"detached"();cBody()] ~struct_name:"vect";
          However this is done automatically from make_explicit_record_assignment transformation
        *)
        (* For expression which are just assignments *)
        make_explicit_record_assignment [cStr "d = p"] ~struct_name:"vect";
        (* An alternative to that is the folowing one:
          make_explicit_record_assignment [cApp ~name:"overloaded=" ~args:[cVar "d" ()] ~validate:(function [true;_] -> true | _ -> false) ()] ~struct_name:"vect";
        *)


        (*
          This returns an error since function calls are not supported

        make_explicit_record_assignment [cStr "d = f()"] ~struct_name:"vect";
          *)

        dump()
    )


(* make explicit record assignment

   t1;
   vect v = { 0, 1 };
   t2;

step1: introduce the block

   t1;
   {
      vect v;
      v.x = 0;
      v.y = 1;
   }@nobrace
   t2;

 step2: eliminate nobrace:

   t1;
   vect v;
   v.x = 0;
   v.y = 1;
   t2;

   make implicit record assingment

   t1;
   vect v;
   v.x = 0;
   v.y = 1;
   t2;

   step1: create subsequence:


   t1;
   subsequence: {
      vect v;
      v.x = 0;
      v.y = 1;
   }@nobrace
   t2;

   step2: call make_implicit_record_assingment_core on subsequence

   t1;
   vect v = { 0, 1 };
   t2;


specification of remove_no_braces:

  seq of the form
   t1;
   { t21; t22; }@no_braces
   t3

   becomes seq
   t1;
   t21;
   t22;
   t3




  v1 = v2
  ->
  v1.x = v2.x
*)