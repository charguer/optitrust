open Optitrust

let _ =
  run
    (fun _ ->
      set_init_source "hello_world.c";
      Label.add "print_instr" [cCall ~name:"printf" ()];
      dump ()
    )
