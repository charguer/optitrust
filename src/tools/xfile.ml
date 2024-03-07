exception Break

let rev_not_rec l =
   let res = ref [] in
   let cur = ref l in
   begin try while true do
      match !cur with
      | [] -> raise Break
      | x::xs ->
         res := x::!res;
         cur := xs
   done with Break -> () end;
   !res

(** Write the string [str] into a file of given name *)

let put_contents filename str =
  let channel = open_out filename in
  output_string channel str;
  close_out channel

(** Write a list of lines into a file of given name *)

let put_lines filename ?(sep="\n") lines =
   put_contents filename (String.concat sep (lines @ [""]))

(** Read the lines of a file; raise FileNotFound if no such file *)

exception FileNotFound of string

let get_lines file =
   if not (Sys.file_exists file)
      then raise (FileNotFound file);
   let lines = ref [] in
   let f =
      try open_in file with End_of_file -> raise (FileNotFound file);
      in
   begin try while true do
      lines := input_line f :: !lines
   done with End_of_file -> () end;
   close_in f;
   rev_not_rec !lines

(** Read the content of a file as a list of lines;
    returns an empty list if no such file exists *)

let get_lines_or_empty file =
   try get_lines file
   with FileNotFound _ -> []

(** Read the content of a file as a string, terminated with a newline;
    raise FileNotFound if no such file exists *)

(* DEPRECATED implementation, slower
let get_contents file =
   let lines = get_lines file in
   (String.concat "\n" lines) ^ "\n" *)
(* LATER: currently the flag newline_at_end is ignored; it should go in a separate function *)
let get_contents ?(newline_at_end:bool=true) file =
  let ic = open_in_bin file in
  let n = in_channel_length ic in
  let text = really_input_string ic n in
  close_in ic;
  text

(** Read the content of a file as a string, terminated with a newline;
    returns an empty string if no such file exists *)

let get_contents_or_empty file =
   try get_contents file
   with FileNotFound _ -> ""

(** Append a string to the end of an existing file *)

let append_contents filename str =
  let contents = get_contents filename in
  put_contents filename (contents^str)


(* [serialize_to filename t]: dumps the object [obj] of type 'a into file [filename]. *)
let serialize_to (filename : string) (obj : 'a) : unit =
  let out_file = open_out_bin filename in
  Marshal.to_channel out_file obj []

(* [unserialize_from filename]: reconstructs the object previously dumped in file [filename]. *)
let unserialize_from (filename : string) : 'a =
  let in_file = open_in_bin filename in
  Marshal.from_channel in_file

(* [is_newer_than filename1 filename2]: checks if the file [filename1] has a modification date greater
    than that of the file [filename2]. *)
let is_newer_than (filename1 : string) (filename2 : string) : bool =
  let t_f1 = Unix.((stat filename1).st_mtime) in
  let t_f2 = Unix.((stat filename2).st_mtime) in
  t_f1 >= t_f2
