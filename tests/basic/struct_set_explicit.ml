open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !!  Struct_basic.set_explicit [sInstr "d = p"];
    !!  Struct_basic.set_explicit [sInstr "u = a.pos"];
    !!  Struct_basic.set_explicit [sInstr "t[0] = p2"];
    !!  Struct_basic.set_explicit [sInstrRegexp ~substr:false "p = ."];
)
(*
    TODO: clean up the cpp file to keep only what is strictly needed


    TODO: document that this operation does not do the detach
     rename to set_explicit_basic

     TODO: set_explicit at the combi level that does the detach automatically.
     try it on:
       vect p = {0,0};
       vect b = p;


START:

   t1;
   vect v = { 0, 1 };
   t2;

FIRST DO "DETACH" operation (either generate a label, or just do i+1 on the path
   in the sequence)

   t1;
   vect v;
   v = { 0, 1 };
   t2;

SET EXPLICIT on the path i+1

   t1;
   vect v;
   v.x = 0;
   v.y = 1;
   t2;



*)