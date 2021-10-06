open Optitrust
open Target

let _ = Run.script_cpp (fun _ -> 

  (* All accesses *)
  show [cAccesses ~accesses:[cAccess] ()];
  (* Array_accesses *)
  show [cIndexGet [cVar "i"]];

  show [cIndexSet [cVar "i"]];

  show  [cAccesses ~base:[cVar "t"] ~accesses:[cField ~field:"i"  ()] ()];
  show  [nbExact 0;cAccesses ~base:[cVar "t"] ~accesses:[cField ~field:"j"  ()] ()];
  
  show  [cAccesses ~accesses:[cField ~field:"p" ~substr:true ()] ()];
  (* Struct accesses *)
  show [cFieldGet "x"];
  show [cFieldGet "y"];
  
  show [cFieldGet "pos"];
  show [cFieldGet "speed"];

  show [cFieldSet "x"];
  show [cFieldSet "y"];
  
  show [cFieldSet "pos"];
  show [cFieldSet "speed"];


  show  [cAccesses ~base:[cVar "p"] ~accesses:[cField ~field:"x"  ()] ()];
  show  [cAccesses ~base:[cVar "p"] ~accesses:[cField ~field:"y"  ()] ()];
  
  show  [cAccesses ~accesses:[cField ~field:"p" ~substr:true ()] ()];
)