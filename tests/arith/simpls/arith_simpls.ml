open Optitrust
open Prelude

let _ = Run.script_cpp (fun () ->

  !! Arith_basic.(simpls ~indepth:true [expand_rec; gather_rec]) [nbMulti; cFunBody "box_filter_rowsum"; cWriteVar "q"; dRHS];
  !! Arith_basic.(simpls ~indepth:true [compute]) [nbMulti; cFunBody "box_filter_rowsum"; cWriteVar "q"; dRHS];
  !! Arith_basic.(simpls [expand; euclidian]) [nbMulti; cFunBody "rowSum"; cAccesses()];
)
