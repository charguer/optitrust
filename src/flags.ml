(* debug printing*)
let verbose : bool ref = ref false

(* interprete dump as dump_trace in transformation scripts *)
let full_dump : bool ref = ref false

(* exit line number *)
let exit_line : int ref = ref max_int

let get_exit_line () : int option =
  if !exit_line = max_int
    then None
    else Some !exit_line

let spec =
  Arg.align [
     ("-verbose", Arg.Set verbose, " activates debug printing");
     ("-exit-line", Arg.Set_int exit_line, " specify the line after which a '!!' symbol should trigger an exit");
     ("-dump-trace", Arg.Set full_dump, " dump ouptputs the full trace in " ^
                                          "transformation scripts");
    ]
