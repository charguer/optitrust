(* debug printing*)
let verbose : bool ref = ref false

(* check the time it takes to run one transformation *)
let analyse_time : bool ref = ref false
let analyse_time_details : bool ref = ref false

(* dump .ast and _enc.cpp files *)
let dump_ast_details : bool ref = ref false

(* Call [Trace.dump_last !dump_last] instead of [Trace.dump], if value is set.
   Note: incompatible with the use of [switch] in scripts, currently. *)
let dump_last_default = -1
let dump_last : int ref = ref dump_last_default

(* Call [Trace.dump_all] in addition to [Trace.dump] *)
let dump_all : bool ref = ref false

(* exit line number *)
let exit_line : int ref = ref max_int

let get_exit_line () : int option =
  if !exit_line = max_int
    then None
    else Some !exit_line

(* ignore small steps to apply multiple transformations at one time *)
let ignore_small_steps : bool ref = ref true

let spec =
  Arg.align [
     ("-verbose", Arg.Set verbose, " activates debug printing");
     ("-exit-line", Arg.Set_int exit_line, " specify the line after which a '!!' symbol should trigger an exit");
     ("-dump-trace", Arg.Set dump_all, " produce a JS file with all the steps performed by the transformation script");
     ("-dump-last", Arg.Set_int dump_last, " dump outputs the number of desired last steps; only for interactive mode");
     ("-dump-ast-details", Arg.Set dump_ast_details, " produce a .ast and a _enc.cpp file with details of the ast");
     ("-analyse_time", Arg.Set analyse_time, " produce a file reporting on the execution time");
     ("-analyse_time_details", Arg.Set analyse_time_details, " produce more details in the file reporting on the execution time (implies -analyse_time)");
     (* LATER: a -dev flag to activate a combination of dump *)
    ]

let fix_flags () =
  if !analyse_time_details then analyse_time := true
