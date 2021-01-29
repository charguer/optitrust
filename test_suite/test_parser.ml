open ScriptTools

let _ =
  run
    (fun () ->
      switch
        [
          (fun () -> set_init_source "test_suite/test.cpp");
          (fun () -> set_init_source "test_suite/test_accesses.cpp");
          (fun () -> set_init_source "test_suite/testPIC/picGoal.cpp")
        ];
      dump ();
      reset ();
      set_init_source "test_suite/testPIC/picGoal_output.cpp";
      dump ();
      let _ =
        Sys.command
          ("meld test_suite/testPIC/picGoal_output.cpp " ^
             "test_suite/testPIC/picGoal_output_output.cpp")
      in
      ()
    )
