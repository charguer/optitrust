open Optitrust
open Prelude

let _ = Flags.check_validity := true

let _ = Run.script_cpp (fun _ ->
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ok1"; cFor "i"];
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ok2"; cLabel "l"];

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ko1"; cFor "i"];
  );
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Variable.local_name ~var:"b" ~local_var:"x" [cFunBody "ko1"; cFor "i"];
  );
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ko2"; cLabel "l"]
  );
  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Variable.local_name ~var:"b" ~local_var:"x" [cFunBody "ko2"; cLabel "l"]
  );

  (* TODO: this triggers a renaming, should it throw an error instead? *)
  !! Variable.local_name ~var:"a" ~local_var:"x"
    [cFunBody "ko_scope"; cLabel "l"];
)
