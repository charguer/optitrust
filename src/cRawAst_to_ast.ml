open Ast

(* environment for storing the mutability of all the variables *)
type env = varkind String_map.t

(* empty environment *)
let env_empty = 
  String_map.empty


(* [is_var_mutable env x] check is variable [x] is mutable or not by searching in [env] *)
let is_var_mutable (env : env) (x : var) : varkind = 
  match String_map.find_opt x env with 
  | Some m -> m 
  | _ -> fail None "is_var_mutable: unbound variable"


(* [env_extend env e varkind] add variable [e] into the environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env = 
  String_map.add e varkind env 


(* [trm_address_of ~simplify t] adds the address operator before [t] 
    if [simplify] is true and [t] is of the form [*u] then it will return just [u]
*)
let trm_address_of ?(simplify : bool = false) (t : trm) : trm = 
  let aux t1 = trm_apps (trm_unop Unop_address) [t1] in
  if not simplify then  aux t 
    else match t.desc with 
    | Trm_apps (f, [t1]) ->
      begin match trm_prim_inv f with 
      | Some (Prim_unop Unop_get) -> t1
      | _ -> aux t 
      end 
    | _ -> aux t 

(* [trm_get ~simplify t] adds the star operator before [t] 
    if [simplify] is true and [t] is of the form [&u] then it will return just [u]
*)
let trm_get ?(simplify : bool = false) (t : trm) : trm = 
  let aux t1 = trm_apps (trm_unop Unop_get) [t1] in
  if not simplify then  aux t 
    else match t.desc with 
    | Trm_apps (f, [t1]) ->
      begin match trm_prim_inv f with 
      | Some (Prim_unop Unop_address) -> t1
      | _ -> aux t 
      end 
    | _ -> aux t 


(* let stackvar_elim (t : trm) : trm = 
  let rec aux (env : env) (t : trm) : trm = 
    match t.desc with 
    | Trm_var x -> 
      if is_var_mutable env x then trm_get t 
      else t 
    | Trm_seq tl -> 
      let process_time (envi, acc) (t1 : trmm) : env * trm list =   
        let (envi2, ti2) =  
          match t1.desc with 
          | Trm_let is_x_mutable (x, ty) tbody -> 
            let ti2 = 
            match (get_inner_ptr_tty).desc with 
            | 
          | _ -> (envi, (aux env ti))
         *)
