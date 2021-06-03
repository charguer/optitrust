
(*
c code


 int main() {
   int a = 5;
   int c = a;
   if (c) { return 1; }
   return 0;
   }

expected c code

 int main() {
   label1: label1bis: int a = 5;
   label2: int c = a;
   label3: if (c) { return 1; }
   return 0;
   }


ml code

  Label.add "label1" [cStr target "int a = 5;"];
  Label.add "label1bis" [cStr "int a"];
  Label.add "label2 [cStr "c = a"];
  Label.add "label3 [cStr "if (c)"];
*)
(*
TRICK for

let unique_id_print = ref false
let unique_id_counter = ref 0

let print_ast_trm
  if !unique_id_print then
     incr unique_id_counter;
     print "@%d" !unique_id_counter
  print annot...

let print_ast ?unique_id=false ()
  unique_id_print := unique_id;
  unique_id_counter := 0;

   print_ast ~only_desc:true ~unique_id:true t foo.ast;
   print_ast ~only_desc:false ~unique_id:true t foo.ast_details;

   @3423 Trm_decl (Def__type)

   @3423 { annot ; ... desc = Trm_decl (Def__type)

*)