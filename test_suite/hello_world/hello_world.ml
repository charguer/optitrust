open Optitrust

let _ =
  run
    (fun _ ->
      set_init_source "hello_world.c";
      add_label "print_instr" [cCall ~name:"printf" ()];
      dump ()
    )
