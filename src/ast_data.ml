open Ast

(* [fun_defs_tbl]: hashtable type, used for storing the function definitions based on their signature. *)
(* type fun_defs_tbl = ((string * typ list), trm) Hashtbl.t *)

type fun_defs_tbl = (Clang.cxcursor, trm) Hashtbl.t

(* [fun_defs] hashtable used for storing function definitions for easier access when needed. *)
let fun_defs : fun_defs_tbl = Hashtbl.create 1000


(* [get_cursor_of_trm t]: returns the Clang.cxcurso id of trm [t] in the case when [t] is a function definition or a function call. *)
let get_cursor_of_trm (t : trm) : Clang.cxcursor option =
  let t_annot = trm_get_cstyles t in 
  List.fold_left (fun acc t_ann -> 
    match t_ann with 
    | Clang_cursor cx ->
      begin match acc with 
      | None -> Some cx
      | _ -> acc
      end
    | _ -> acc
  
  ) None t_annot


(* [fill_fun_defs_tbl t]: traverses the ast [t] and adds into the table [fun_defs] all the function definitions.
      with keys being their original Clang.cxcursor id. *)
let fill_fun_defs_tbl () : unit =
  (* First clean the current hashtable. *)
  let ast = Target.get_ast() in
  Hashtbl.clear fun_defs;
  let rec aux (t : trm) : unit =
    match t.desc with 
    | Trm_let_fun (qf, ret_ty, args, body) -> 
      begin match get_cursor_of_trm t with 
      | Some cx -> Hashtbl.add fun_defs cx t
      | None -> () (* Maybe it shoudl fail here! *)
      end
    | Trm_typedef td -> trm_iter aux t
    | _ -> trm_iter aux t
   in 
   aux ast

(* [get_function_def t]: assumes that [t] is the callee of a function call, annotated with the Clang cxcursor.
    Then it will return the definition of the function whose name appears in [ŧ]. *)
let get_function_def (t : trm) : trm = 
  match get_cursor_of_trm t with 
  | Some cx -> 
    begin match Hashtbl.find_opt fun_defs (Clang.get_cursor_referenced cx) with 
    | Some fun_def -> fun_def
    | None -> fail t.loc "Ast.get_function_def: couldn't find the definition of the called function"
    end
  | None -> fail t.loc "Ast.get_function_def: expected a trm annotated with Clang cxcursor."

