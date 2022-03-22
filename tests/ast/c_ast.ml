(* This test is just to see if the AST is printed back as expected.
   The c_ast_out_enc.cpp file shows the encoded version. *)
open Optitrust
open Target

let _ =
  Flags.use_light_diff := false

let _ = Run.script_cpp ~parser:Parsers.Clang (fun () ->

  !! Trace.reparse ~parser:Parsers.Clang ();

  !! Trace.reparse ~parser:Parsers.Menhir (); (* F6 on this line shows the difference between Clang and Menhir *)

  !! Trace.alternative (fun () ->
    !! Trace.reparse ~parser:Parsers.All (); (* F6 on this line checks for discrepencies *)
  );

)


(* TODO: move to the c_ast, and support also with clang
int main(void)
{
  int count = 0;
  #pragma omp parallel for
  for (int n = 0; n < 8; n++) {
    #pragma omp atomic
    count++;
  }
  #pragma omp parallel for collapse(2)
  for (int a = 0; a < 8; a++) {
    for (int b = 0; b < 8; b++) {
    #pragma omp atomic
    count++;
    }
  }
  return 0;
}
*)
