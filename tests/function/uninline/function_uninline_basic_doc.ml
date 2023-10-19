
open Optitrust
open Target

(* Note: In C11, any parameter of function type is adjusted to the corresponding pointer type. Eg:
    int f(char g(double)); // declares int f(char ( *g )(double))
    int h(int(void)); // declares int h(int ( * )(void))
   We favor the lightweight notation on the left-hand side, to improve readability;
   in the future, we may want to introduce an annotation to allow preserving the presentation
  used by the original code in case it involves a star. *)

let _ = Run.script_cpp (fun _ ->

   !! Function_basic.uninline ~fct:[cFunDef "g"] [cLabel "body"];

)
