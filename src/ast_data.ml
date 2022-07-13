open Ast

(* [fun_defs_tbl]: hashtable type, used for storing the function definitions based on their signature. *)
type fun_defs_tbl = ((string * typ list), trm) Hashtbl.t


(* [fun_defs] hashtable used for storing function definitions for easier access when needed. *)
let fun_defs : fun_defs_tbl = Hashtbl.create 1000


(* [fill_fun_defs_tbl t]: traverses the ast [t] and adds into the table [fun_defs] all the function definitions. *)
let fill_fun_defs_tbl (t : trm) : unit =
  let rec aux (class_name : var) (t : trm) : unit =
    match t.desc with 
    | Trm_let_fun (qn, ret_ty, args, body) when not (is_trm_uninitialized body)-> 
       let args_types = List.map snd args in 
       let qf = if class_name <> "" then qvar_update ~qpath:(class_name :: qn.qvar_path) qn else qn in 
       let fun_name = qf.qvar_str in 
       Hashtbl.add fun_defs (fun_name, args_types) body
    | Trm_typedef td -> trm_iter (aux td.typdef_tconstr ) t
    | _ -> trm_iter (aux class_name) t
   in 
 aux "" t
