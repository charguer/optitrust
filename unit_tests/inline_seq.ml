open ScriptTools

let _ = run_unit_test (fun () ->
  (* TODO: this is one command that finds a sequence, however it leaves a bunch of unexpected delete operations *)
  (* inline_seq  ~seq_path:[[cVarDef ~name:"x" ()] >>! []] ();*)
  ()
)

