open OptiTrust
open Target


let _ = Run.script_cpp (fun _ -> 

  !! Function_basic.dsp_def [cFunDef "f"];


)