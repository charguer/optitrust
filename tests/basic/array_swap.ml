open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Arrays_basic.swap [cTypDef "T"];
  )
"
typedef int T[2][3];

int main() {
  T t;
  int a = t[0][1];
}
"


let _ = Run.script_cpp (fun _ ->
    !! Arrays_basic.swap [cTypDef "T"];
)j