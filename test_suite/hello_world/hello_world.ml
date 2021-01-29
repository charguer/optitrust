open ScriptTools

let _ =
  run
    (fun _ ->
      set_init_source "hello_world.c";
      add_label "print_instr" [cApp ~name:"printf" ()];
      dump ()
    )
