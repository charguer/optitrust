open Syntax

(* [fun_defs_tbl]: hashtable type, used for storing the function definitions based on their signature.
    The key is the Unified Symbol Resolution.
    Using Clang.cxcursor instead is not possible because all Clang.cxcursor give the same key *)
type fun_defs_tbl = (string, trm) Hashtbl.t

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

(* [get_cursor_at_trm_unsome t]: similar to [get_cursor_at_trm] but this one fails in case the trm [t] doesn't have  clang_id.   *)
let get_cursor_of_trm_unsome (t :trm) : Clang.cxcursor =
  match get_cursor_of_trm t with
  | Some cx -> cx
  | None -> assert false

(* [get_function_usr t]: assume that t is the callee of a funcall or a function definition, annotated with the Clang cxcursor.
    Then it will return the Unified Symbol Resolution of the function. *)
let get_function_usr (t : trm) : string option =
  match get_cursor_of_trm t with
  | Some (cx) ->
    begin match Clang.get_cursor_usr (Clang.get_cursor_definition cx) with
    | "" -> None
    | usr -> Some (usr)
    end
  | None -> None

(* [get_function_usr t]: assume that t is the callee of a funcall or a function definition, annotated with the Clang cxcursor.
    Then it will return the Unified Symbol Resolution of the function. *)
let get_function_usr_unsome (t : trm) : string =
  match get_function_usr t with
  | Some (usr) -> usr
  | None -> assert false

(* [fill_fun_defs_tbl t]: traverses the ast [t] and adds into the table [fun_defs] all the function definitions.
      with keys being   their original Clang.cxcursor id. *)
let fill_fun_defs_tbl (t : trm) : unit =
  (* First clean the current hashtable. *)
  let debug = ref false in
  Hashtbl.clear fun_defs;
  let rec aux (t : trm) : unit =
    match t.desc with
    | Trm_let_fun (qf, ret_ty, _args, body, _) ->
      begin match get_function_usr t with
      | Some usr -> Hashtbl.add fun_defs usr t
      | None -> () (* Maybe it shoudl fail here! *)
      end
    | Trm_typedef td -> trm_iter aux t
    | _ -> trm_iter aux t
   in
   let res = aux t in
   if !debug then Hashtbl.iter (fun _k v -> Printf.printf "Value: %s\n" (AstC_to_c.ast_to_string v)) fun_defs;
   res

(* [get_function_def t]: assumes that [t] is the callee of a function call, annotated with the Clang cxcursor.
    Then it will return the definition of the function whose name appears in [ŧ]. *)
let get_function_def (t : trm) : trm =
  match get_function_usr t with
  | Some (usr) ->
    begin match Hashtbl.find_opt fun_defs usr with
    | Some fun_def -> fun_def
    | None -> fail t.loc "Ast_data.get_function_def: couldn't find the definition of the called function"
    end
  | None -> fail t.loc "Ast_data.get_function_def: expected a trm annotated with Clang cxcursor."

