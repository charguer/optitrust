open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->

  !! Arith_basic.(simpls [expand; euclidian]) [nbMulti; cAccesses()];
  (*
  !! Arith_basic.(simpl ~indepth:true expand) [nbMulti; cAccesses()];
  !! Arith_basic.(simpl ~indepth:true euclidian) [nbMulti; cAccesses();
   !! ()
   *)
)
