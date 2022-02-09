open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
  !! Struct.set_explicit [sInstr "v = w"];
  )
"
typedef struct {
    int x;
    int y; }
  vect;

int main() {
  vect v;
  vect w;
  v = w;
}
"


let _ = Run.script_cpp (fun _ ->
    (* Example with detach of initialization *)
    !! Struct.set_explicit [cVarDef "p"];
    (* Another example with more complex initializers *)
    !! Struct.set_explicit [sInstr "obj a = "];
    !! Struct.set_explicit [sInstr "a.speed = "];
    (* Another example with a more complex right-hand side *)
    !! Struct.set_explicit [cVarDef "u"];
    (* Example without detach *)
    !! Struct.set_explicit [sInstr "b = p"];
)
