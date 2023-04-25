open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

  !! Record_basic.set_explicit [sInstr "a = b"];

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

  !! Record_basic.set_explicit [sInstr "b = p"];
  !! Record_basic.set_explicit [sInstr "u = a.pos"];
  !! Record_basic.set_explicit [sInstr "t[0] = p2"];
  !! Record_basic.set_explicit [sInstr "c = a"];
  !! Record_basic.set_explicit [sInstr "c.pos ="];
  !! Record_basic.set_explicit [sInstr "c.speed ="];


  !! Trace.alternative (fun () ->
    !! Record_basic.set_explicit [sInstr "c = a"];
    !! Record_basic.set_explicit [nbMulti;cOr [[sInstr "c.pos ="]; [sInstr "c.speed ="]]];
    !! ());

)
