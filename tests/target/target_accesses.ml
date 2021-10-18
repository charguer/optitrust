open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  (* All accesses *)
  show [cAccesses ~accesses:[cAccess] ()];
  
  (* Array_accesses *)
  show [cCellRead [cVar "i"]];

  (* show [cCellWrite [cVar "i"]]; *)

  show  [cAccesses ~base:[cVar "t"] ~accesses:[cIndex ~index:[cVar "i"]  ()] ()];
  show  [nbExact 0;cAccesses ~base:[cVar "t"] ~accesses:[cIndex ~index:[cVar "j" ] ()] ()];
  
  show  [cAccesses ~accesses:[cField ~field:"x" ~substr:true ()] ()];
  (* Struct accesses *)
  show [cFieldRead "x"];
  show [cFieldRead "y"];
  
  show [cFieldRead "pos"];
  show [cFieldRead "speed"];

  show [cFieldWrite "x"];
  show [cFieldWrite "y"];
  
  show [cFieldWrite "pos"];
  show [cFieldWrite "speed"];


  show  [cAccesses ~base:[cVar "p"] ~accesses:[cField ~field:"x"  ()] ()];
  show  [cAccesses ~base:[cVar "p"] ~accesses:[cField ~field:"y"  ()] ()];
  
  show  [cAccesses ~accesses:[cField ~field:"p" ~substr:true ()] ()];
)