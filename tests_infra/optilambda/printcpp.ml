open Optitrust
open Target
module OL = Optitrust_optilambda.Optilambda

let _ = Run.script_cpp (fun _->

  let trm = Trace.ast() in
  File.put_contents "./tests_infra/optilambda/printcpp_out.opti"
  (Printf.sprintf "-- %s --\n%s\n\n" "CPP test" (OL.trm_to_string trm))

)
