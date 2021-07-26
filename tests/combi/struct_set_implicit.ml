open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
  !! Struct.set_explicit [sInstr ~substr:false "b.x = p.x"];
)

(*
  TODO: document that we assume the block
  TODO: rename to set_implicit_basic

  TODO: implement set_implicit which call Sequence.sub with the right arguments
  then calls set_implicit_basic

   t1;
   v.x = 0;
   v.y = 1;
   t2;

   give the path to the first set operation (v.x = 0)
   then use the type information of "v" to find out the number of fields
   check that the sequence after v.x contains one set operation for each field exactly,
   in the right order.

   t1;
   v = { 0 , 1 };
   t2;

   LATER: we will implement a function that reorders a list of set operations
   in the right order, and check that they are no "bad" effects that get swapped.



   IF YOU RECOGNIZE THE PATTERN
   t1;
   v.x = p.x;
   v.y = p.y;
   t2;

  THEN DON'T GENERATE
   t1;
   v = { p.x , p.y };
   t2;

  BUT INSTEAD
   t1;
   v = p;
   t2;
*)
