let _ =
  Arg.parse
    ScriptTools__Flags.spec
    (fun s ->
      let prog = "test_suite/" ^ s ^ ".exe" in
      let _ =
        Sys.command ("dune exec " ^ prog ^ " -- -verbose")
      in ()
    )
    "usage: make TESTS=\"test_name1 test_name2 ...\" tests"
