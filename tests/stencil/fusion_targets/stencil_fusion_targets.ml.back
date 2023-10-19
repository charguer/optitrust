open Optitrust
open Target
open Prelude

let _ = Run.doc_script_cpp (fun _ ->
  ()(* TODO *)
)

"
int main() {
  int x = 0;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j += 2) {
      x++;
    }
  }
  int y = 0;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 5; j += 2) {
      y++;
    }
  }
}
"


let _ = Run.script_cpp ( fun _ ->
  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"] [cFunBody "add2"; nbMulti; cFun "add"];
  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"] [cFunBody "add2vbox"; multi cFun ["vbox"; "add"]];
  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"]
    ~overlaps:["ab", [trm_int 0; trm_int 2]]
    [cFunBody "vboxadd"; multi cFun ["vbox"; "add"]];
  !! Stencil.fusion_targets_tile [trm_int 32]
    ~outputs:["out"]
    ~overlaps:["ab", [trm_int 2]]
    [cFunBody "hboxadd"; multi cFun ["hbox"; "add"]];
)
