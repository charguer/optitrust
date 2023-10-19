open Optitrust
open Target

(* TODO: make it clearer what this transfo is expected to do;
   should it have a different name than aos_to_soa? *)

(* TODO: This transfo does not seem to be correct *)

(* TODO: later add a doc

let _ = Run.doc_script_cpp (fun _ ->
    !! Arrays_basic.aos_to_soa "vects" "B";
  )
"
typedef struct {
  int x;
  int y;
} vect;

vects vs[10];

int main() {
  int a = vs[3].x;
}
"
*)

let _ = Run.script_cpp (fun () ->
  !! Arrays_basic.aos_to_soa "vects" "B";
)
