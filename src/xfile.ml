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

let get_contents ?(newline_at_end:bool=true) file =
   let lines = get_lines file in
   let lines = if newline_at_end then lines @ [""] else lines in
   (String.concat "\n" lines)

(** Read the content of a file as a string, terminated with a newline;
    returns an empty string if no such file exists *)

let get_contents_or_empty file =
   try get_contents file
   with FileNotFound _ -> ""

(** Append a string to the end of an existing file *)

let append_contents filename str =
  let contents = get_contents filename in
  put_contents filename (contents^str)

