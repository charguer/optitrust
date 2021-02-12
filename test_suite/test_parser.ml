open ScriptTools

let _ =
  run
    (fun () ->
      switch
        [
          (fun () -> set_init_source "test/test.cpp");
          (fun () -> set_init_source "test_accesses/test_accesses.cpp");
          (fun () -> set_init_source "testPIC/picGoal.cpp")
        ];
      dump ();
      reset ();
      set_init_source "testPIC/picGoal_output.cpp";
      dump ();
      let _ =
        Sys.command
          ("meld testPIC/picGoal_output.cpp " ^
             "testPIC/picGoal_output_output.cpp")
      in
      ()
    )
