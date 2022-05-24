open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

   !! Loop_basic.unswitch [cIf ~cond:[sExpr "b"] ()];

)

"
#include <stdbool.h>
bool b;

int main() {
  int s;
  int t;
  for (int i = 0; (i < 3); i++) {
    if (b) {
      s += i;
    } else {
      t += i;
    }
  }
}
"

let _ = Run.script_cpp (fun _ ->
  !! Loop_basic.unswitch [cIf ~cond:[cBool true] ()];
  !! Loop_basic.unswitch [cIf ~cond:[cBool false] ()];
)

(* LATER: generalize the transformation to arbitrary context

    E[ (if b then t1 else t2) ]
  -->
    if b then E[t1] else E[t2]

    provided that b does not depend on the variables introduced in the context E.

    we could give a path to the root of the context, and a path to the if-statement inside it
*)
