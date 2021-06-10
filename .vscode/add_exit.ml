let filename : string ref = ref ""

let line : int ref = ref 0

let spec : (Arg.key * Arg.spec * Arg.doc) list =
  Arg.align [
      ("-file", Arg.Set_string filename, " transformation script filename");
      ("-line", Arg.Set_int line, "line where to add the exit point")
    ]

(* return the list of lines in the file *)
let get_lines (file : string) : string list =
   (* if not (Sys.file_exists file)
    *    then raise (FileNotFound file); *)
   let lines = ref [] in
   let f =
      (* try *) open_in file (* with End_of_file -> raise (FileNotFound file); *)
   in
   begin try while true do
      lines := input_line f :: !lines
   done with End_of_file -> () end;
   close_in f;
   List.rev !lines

(*
  add [Trace.dump_diff_and_exit] instruction to the line:
  - bla -> (bla; Trace.dump_diff_and_exit ())
  - bla; -> (bla; Trace.dump_diff_and_exit ());
 *)
let add_exit (line : string) : string =
  match String.split_on_char ';' line with
  | [s] -> "(" ^ s ^ "; Trace.dump_diff_and_exit ())"
  | sl ->
     let sl' = List.rev sl in
     let prefix = String.concat (String.make 1 ';') (List.rev (List.tl sl')) in
     let suffix = List.hd sl' in
     "(" ^ prefix ^ "; Trace.dump_diff_and_exit ());" ^ suffix

(* replace the n-th element of al with a *)
let rec update (n : int) (a : 'a) (al : 'a list) : 'a list =
  match al with
  | [] -> []
  | a' :: al' -> if n <= 0 then a :: al' else a' :: update (n - 1) a al'

(* print the lines in the file *)
let output_lines (file : string) (lines : string list) : unit =
  let c_out = open_out file in
  List.iter (fun s -> output_string c_out (s ^ "\n")) lines;
  close_out c_out

let _ =
  Arg.parse spec (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    "usage: ocaml add_exit.ml -file f -line l";
  let sl = get_lines !filename in
  let s = List.nth sl (!line - 1) in
  let s' = add_exit s in
  let sl' = update (!line - 1) s' sl in
  output_lines (Filename.remove_extension !filename ^ "_with_exit" ^
                  Filename.extension !filename) sl'
