open Optitrust
open Prelude

let _ = Flags.check_validity := false

let _ = Run.script_cpp ( fun _ ->
  (* !! Resources.ensure_computed (); *)
  !! Resources.delete_annots [];

  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"] [cFunBody "add2"; nbMulti; cCall "add"];
  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"] [cFunBody "add2vbox"; multi cCall ["vbox"; "add"]];
  !! Stencil.fusion_targets ~nest_of:2 ~outputs:["out"]
    ~overlaps:["ab", [trm_int 0; trm_int 2]]
    [cFunBody "vboxadd"; multi cCall ["vbox"; "add"]];
  !! Stencil.fusion_targets_tile [trm_int 32]
    ~outputs:["out"]
    ~overlaps:["ab", [trm_int 2]]
    [cFunBody "hboxadd"; multi cCall ["hbox"; "add"]];
)
