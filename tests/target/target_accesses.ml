open Optitrust
open Prelude
let show = Show.add_marks_for_target_unit_tests

let _ = Run.script_cpp (fun _ ->

  (* All accesses *)
  !! show [cAccesses ~accesses:[cAccess] ()];

  (* Array_accesses *)
  !! show [cCellRead ()];

  !! show [cCellWrite ~index:[cVar "i"] ()];

  !! show  [cAccesses ~base:[cVar "t"] ~accesses:[cIndex ~index:[cVar "i"]  ()] ()];
  !! show  [nbExact 0;cAccesses ~base:[cVar "t"] ~accesses:[cIndex ~index:[cVar "j" ] ()] ()];

  !! show  [cAccesses ~accesses:[cField ~field:"x" ~substr:true ()] ()];
  (* Struct accesses *)
  !! show [cFieldRead ~field:"x" ()];
  !! show [cFieldRead ~field:"y" ()];

  !! show [cFieldRead ~field:"pos" ()];
  !! show [cFieldRead ~field:"speed" ()];

  !! show [cFieldWrite ~field:"x" ()];
  !! show [cFieldWrite ~field:"y" ()];

  !! show [cFieldWrite ~field:"pos" ()];
  !! show [cFieldWrite ~field:"speed" ()];


  !! show  [cAccesses ~base:[cVar "p"] ~accesses:[cField ~field:"x"  ()] ()];
  !! show  [cAccesses ~base:[cVar "p"] ~accesses:[cField ~field:"y"  ()] ()];

  !! show  [cAccesses ~accesses:[cField ~field:"p" ~substr:true ()] ()];
)
