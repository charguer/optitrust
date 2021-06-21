open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
        

        (* show [cInstr "d = p"]; *)
        Struct.set_explicit [cInstr "d = p"];
        Struct.set_explicit [cInstr "u = a.pos"];
        Struct.set_explicit [cInstr "t[0] = p2"];(* Doesn't work*)
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