open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  
  (* With partitioning *)
  !! Loop.unroll ~braces:false ~shuffle:true ~blocks:[2;3] [cFor "i"];
  !! Loop.unroll ~braces:false  [cFor "j"];
  
  (* Without partitioning *)
  !! Trace.alternative (fun _ -> 
    !! Loop.unroll ~braces:true [cFor "i"];
    !! (););

  (* Hiding braces *)
  !! Trace.alternative (fun () ->
    !! Loop.unroll [cFor "i"];
    !!())
)


(* DONE:
   in the combi level, [unroll] is matching the code

       for (int i = a; i < a + N; i++) {

    if N is a variable -> try to call inline_var on this target
    then
    if N is not a literal -> fail
    then
    call the basic unroll


    [unroll_and_shuffle] which does unroll, then [shuffle]

    [shuffle] is a stand alone transformation (see notes)
*)

(* STEP 1 (BASIC): ONLY UNROLL

   for i { body(i) }
   --->
   { body(i+0) }
   { body(i+1) }
   { body(i+2) }

  example:
    { int a = (i+0 * 2);
        t[i] = a; }
    { int a = (i+1 * 2);
        t[i] = a; }

  STEP2:  software-pipelining is a combi transformation that decomposes as:

   START:
   {
     { body(i+0) }
     { body(i+1) }
     { body(i+2) }
   }

   FIRST SUBSTEP : perform renaming of local varaibles (see simd.txt)

   SECOND SUBSTEP: make the subgroups
    now with number of instructions in each sublock, e.g. take a list [2;3]
     Sequence.partition [2;3] p    // DONE: test "partition" as a combi transfo
        // -> check that the sum of the sizes in the list correspond to the nb of items in the seq
       -> implemented as
            Sequence.sub 0 2; Sequence.sub 1 3; Sequence.sub 2 ...
          (list fold over the partition sizes)
       -> make the @nobraces on the subsequences produced (this should be a flag of Seq.sub),
          so that we can remove them at the end
       where p points to the item "body(i+k)"

       ( if body(i) is   instr1 instr2 instr3 instr4 instr5
       ( then i make { { instr1 instr2 } { instr3 instr4 instr5 } }

   {
     { { instr1 instr2(i+0) } { instr3 instr4 instr5(i+0) } }
     { { instr1 instr2(i+1) } { instr3 instr4 instr5(i+1) } }
     { { instr1 instr2(i+2) } { instr3 instr4 instr5(i+2) } }
   }
   THIRD SUBSTEP: reorder instructions
   {
     { { instr1 instr2(i+0) }@nobrace
       { instr1 instr2(i+1) }
       { instr1 instr2(i+2) } }@?
     { { instr3 instr4 instr5(i+0) }
       { instr3 instr4 instr5(i+1) }
       { instr3 instr4 instr5(i+2) } }@?
   }

   FOURTH SUBSTEP: remove nobrace sequences


 ===================note
    the actual reorder operation is just (the one already implemented):
    {
     { cmd1(i+0) cmd2 cmd3 }
     { cmd1(i+1) cmd2 cmd3 }
     { cmd1(i+2) cmd2 cmd3 }
   }
   THIRD SUBSTEP: reorder instructions
   {
     cmd1(i+0)
     cmd1(i+1)
     cmd1(i+2)
     cmd2(i+0)
     cmd2(i+1)
     cmd2(i+2)
     cmd3(i+0)
     cmd3(i+1)
     cmd3(i+2)

   }

*)