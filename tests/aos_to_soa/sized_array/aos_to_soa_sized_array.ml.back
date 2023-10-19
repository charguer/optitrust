open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Arrays_basic.aos_to_soa "vect" "4";
  )
"
typedef struct {
  int x;
  int y;
} vect;
vect u[4];
int main() {
  int a = u[0].x;
  int b = u[1].y;
}
"

let _ = Run.script_cpp (fun () ->
  !! Arrays_basic.aos_to_soa "vect2" "N";
)
