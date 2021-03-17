open ScriptTools



let _ = 
    run 
    ( fun _ -> 
        set_init_source"inline_struct.cpp";
        inline_struct "obj" ~struct_fields:["pos"];
        inline_decl ~delete_decl:true ~decl_path:[cVarDef ~name:"s" ()] (); 
           (* TODO: inline_decl_struct.ml 
              rename inline_var to inline_decl_var 
              rename inline_fun to inline_decl_fun 
              rename inline_typ to inline_decl_typ *)
              (* inline_decl_struct.ml 
                  vect s = { 0, 0 };
                  vect a = { p, s, 1 }; // here should succeed
                  
                  vect t = { 0, 0 };
                  int b = t.x;  // is this going to be supported? ideally, could produce int b = 0.
              *)
              (* 
              *)
        
        (*(* internal not in scriptTools.mli *) inline_one_struct_in_struct ~name:"obj" "pos";
        inline_struct_in_struct ~name:"obj" ["pos"];
        inline_struct_in_struct ~name:"obj" ["pos" ;"speed"]; *) 
        dump()
    )

(*
  obj a = {a, {0,0}, s}   let i be the index of where "pos" was in "obj"
  obj a = {a, 0,0, s}    obtained by inlining the sublist at index i into the main list
    
 
  obj p = {0, 0 }
  obj a = { 1, p, s}

 inlining when the sublist is not visible
  obj a' = { 1, p.x, p.y, s }
 
   Yet another complication
    obj a = { 1, f(), s }
    obj a = { 1, f().x, f().y, s } // NOT SAFE TRANSFORMATION
    // only variables are safe


  Complications:

      vect p = { 0, 0 }:
      p.x = 3;
      obj a = { 1, p, s };

      obj a. = { 1, p.x, p.y, s } // seems to be safe

  Anoter complication

     vect p = { 0, f() }: // where f has side effects
     obj a = { 1, p, s };
     obj b = { 1, p, s }; 

    user could do, either before of after
      int y= f()  // introducing a definitiion / folding
      vect p = { 0, y }: // then p can be inlined at all its occurences because it's made of constants
      obj a = { 1, p.x, p.y, s };
      obj b = { 1, p.x, p.y, s }; 

   // in this case the useful transformation is "inlining of a record def"
   //  vect p = { c1, c2 } // constants fields
   //  p.x -> c1
   //  p.y -> c2
   // if i find p without a projection "p" then either "leave p" // or raise error. [FOR NOW raise error]

  Even in simple case:
    vect p = { 0, 0 }:  // should this decl be deleted?
    obj a = { 1, p, s }; // assume p only occurence

   // either the use does inline p first:
   // obj a = { 1 , { 0, 0 }, s }
   //  in which case

   // or the user inlines p only later:
   // obj a = { 1, p.x, p.y, s };

     // { 0, 0 }.x  => this is allowed in our AST but is not valid C syntax, so we should never generate it




go over places such that   trm_struct el when t.typ = "obj" ->
  call inline_sublist_in_list



  any operation on a record involves:
  1) typedef        typedef struct { .. } post :DONE
  2) accesss_get    a.pos .DONE
  3) access_set     a.pos DONE
  4) initialization    { 0, 0 } DONE
  5) new               new obj //probably no change needed


  // make_explicit_record_assignment  (here path =  cAccessSet "pos")
  obj.pos =  v
  ->
  obj.pos.x = v.x
  obj.pos.y = v.y


  obj.pos.x 

  // pacakge function:
//  -1. make_explicit_record_assignment  (here path =  cAccessSet "pos")
// 2. inlining of struct
// 3  automatically inline & simplifiy definition  { 1, p.x, p.y, s }  if p is defined as { a, b }
    then this can be simplified to { 1, a ,b, s } and removing p
    // TASK: fix inline_def transformation so that when the body (what you inline) is a trm_struct,
      then check that you can take the projections out, otherwise error.
      //  access_field ?f (tmr_var p)  -->  replace directly by field f of the body
//   
*)