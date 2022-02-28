open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->


  let ctx = cTopFunDef "cornerInterpolationCoeff" in
  let ctx_rv = cChain [ctx; sInstr "r.v"] in
  !! Variable.intro_pattern_array ~const:true ~pattern_aux_vars:"double rX, rY, rZ"
      ~pattern_vars:"double coefX, signX, coefY, signY, coefZ, signZ"
      ~pattern:"(coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ)"
      [nbMulti; ctx_rv; dRHS];   
)

