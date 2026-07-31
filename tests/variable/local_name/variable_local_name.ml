open Optitrust
open Prelude

let _ = Flags.typechecking_mode := Flags.AnnotatedAndVerified

let _ = Run.script_cpp (fun _ ->
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ok1"; cFor "i"];
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ok2"; cLabel "l"];

  (* !! Trace.resource_error_expected (fun () -> *)
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ko1"; cFor "i"];
  (* ); *)
  !! Trace.resource_error_expected (fun () ->
    Variable.local_name ~var:"b" ~local_var:"x" [cFunBody "ko1"; cFor "i"];
  );
  !! Trace.resource_error_expected (fun () ->
    Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ko2"; cLabel "l"]
  );
  !! Trace.resource_error_expected (fun () ->
    Variable.local_name ~var:"b" ~local_var:"x" [cFunBody "ko2"; cLabel "l"]
  );

  (* TODO: this triggers a renaming, should it throw an error instead? *)
  !! Variable.local_name ~var:"a" ~local_var:"x"
    [cFunBody "ko_scope"; cLabel "l"];

  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ok3"; tSpanSeq [cForBody "i"]];

  (* !! Show.At.trm ~style:(Style.internal_ast_only_desc ()) [cFunBody "ok4"];
  !! Marks.add "end" [nbMulti; tAfter; cFunBody "ok4"; cCall ~args:[[cPrimCall ~args:[[cVar "a"]] (Prim_unop Unop_post_incr)]] "__ignore"]; *)
  !! Variable.local_name ~var:"a" ~local_var:"x" [cFunBody "ok4"; tSpan [tBefore; cVarDef "b"] [tAfter; cInstr [cPrimCall ~args:[[cVar "a"]] (Prim_unop Unop_post_incr)]]];
)
