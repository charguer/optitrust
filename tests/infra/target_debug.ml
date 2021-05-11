open Optitrust

let _ = run_unit_test (fun () ->
  let show = show_target in

  show [ cIf() ];

  (* show [ cReturn() ];*)

)
