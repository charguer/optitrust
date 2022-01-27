open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
     Variable_basic.inline [cVarDef "v"];
  !! Struct_basic.simpl_proj [cVarDef "a"];
  )
"
typedef struct {
  int x;
  int y;
} vect;

int main() {
  const vect v = { 1, 2 };
  int a = v.x;
  int b = v.y;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [cFun "vect_add"];
  !! Struct_basic.simpl_proj [cFunDef "main"];
)

(* TODO: the diff are all empty it seems *)