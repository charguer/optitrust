open Optitrust
open Run

let exit_at_line = ref 16

(* An identity function that exits the program if the line argument exceeds the
   value of the global variable [exit_at_line], obtained from the command-line *)

let (!!) (line : int) : unit =
  Printf.printf "line %d \n" line;
  if line > !exit_at_line
    then (dump(); exit 0);
 Printf.printf "continue\n"

let _ = run_unit_test (fun () ->
  !! __LINE__; show_target [cVar "a"];
  !! __LINE__; (let p = [cVar "b"] in
  show_target p);
)
