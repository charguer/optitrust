open Ast

(* [fun_defs_tbl]: hashtable type, used for storing the function definitions based on their signature. *)
(* type fun_defs_tbl = ((string * typ list), trm) Hashtbl.t *)

type fun_defs_tbl = (Clang.cxcursor, trm) Hashtbl.t

(* [fun_defs] hashtable used for storing function definitions for easier access when needed. *)
let fun_defs : fun_defs_tbl = Hashtbl.create 1000

(* [fill_fun_defs_tbl t]: traverses the ast [t] and adds into the table [fun_defs] all the function definitions.
      with keys being their original Clang.cxcursor id. *)
let fill_fun_defs_tbl (t : trm) : unit =
  let rec aux (t : trm) : unit =
    match t.desc with 
    | Trm_let_fun (qf, ret_ty, args, body) -> 
      begin match get_cursor_of_decl t with 
      | Some cx -> Hashtbl.add fun_defs cx t
      | None -> () (* Maybe it shoudl fail here! *)
      end
    | Trm_typedef td -> trm_iter aux t
    | _ -> trm_iter aux t
   in 
   aux t
