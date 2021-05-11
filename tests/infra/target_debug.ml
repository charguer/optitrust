open Optitrust

let _ = run_unit_test (fun () ->
  let show = show_target in
  set_repeat_io false;
  (*show [ cIf() ];*)

  show [ cMulti; cReturn() ];

)
