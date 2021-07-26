open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
   !! Variable_basic.inline ~at:[[cVarDef "b"]] [cVarDef "a"];
   !! Variable_basic.inline ~delete:true [cVarDef "c"];
   !! Variable_basic.inline ~at:[[cVarDef "y"]] [cVarDef "x"];
   !! Variable_basic.inline ~delete:true [cVarDef "z"];

   (* FIXED: rename delete_decl to delete or remove *)
   (* VERY MUCH LATER:
       at the basic level: apply inlining
       at the high level:
       - always ok to inline arithmetic expressions (possibly hidden behind functions)
       - operations with potential side effects should not be duplicated
         except if there is a single occurrence of the variable
         AND this occurence is not beyond other interacting side effects

      const int a = x++;
      int b = a;
      int c = a;
      => not safe to inline a

      const int a = x++;
      int b = a;
      => safe to inline a

      const int a = x++;
      int c = y;
      int b = a;
      => safe to inline a

      const int a = x++;
      int c = x++;
      int b = a;
      => not safe to inline a

      const int a = x++;
      int b = f(x++,a);
      => not safe to inline a
    *)
)
