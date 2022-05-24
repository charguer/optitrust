open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

    !! Struct_basic.set_explicit [sInstr "a = b"];

)

"
typedef struct {
  int x;
  int y;
} vect;

int main() {
  vect a;
  vect b;
  a = b;
}
"
let _ = Flags.dump_ast_details := true


let _ = Run.script_cpp ( fun _ ->

  !! Struct_basic.set_explicit [sInstr "b = p"];
  !! Struct_basic.set_explicit [sInstr "u = a.pos"];
  !! Struct_basic.set_explicit [sInstr "t[0] = p2"];
  !! Struct_basic.set_explicit [sInstr "c = a"];
  !! Struct_basic.set_explicit [sInstr "c.pos ="];
  !! Struct_basic.set_explicit [sInstr "c.speed ="];


  !! Trace.alternative (fun () ->
    !! Struct_basic.set_explicit [sInstr "c = a"];
    !! Struct_basic.set_explicit [nbMulti;cOr [[sInstr "c.pos ="]; [sInstr "c.speed ="]]];
    !! ());
)
