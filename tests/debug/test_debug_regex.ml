open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    
  show [sInstrRegexp ~substr:true "res.*\\[0\\]"];
  !! Expr.view_subterms ~constr:(sInstr "res.x = res.x + coeffs.v[0]") [dRoot];
  !! Instr.accumulate ~nb:8 [nbMulti; sInstrRegexp "res.*\\[0\\]"];	

)