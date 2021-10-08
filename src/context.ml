open Ast

(* [typid_to_typedef tid ] Get the declaration of a typedef with id [tid]*)
let typid_to_typedef (tid : typconstrid) : typedef option =
  let t_root = Trace.ast () in
  match t_root.desc with 
  | Trm_seq tl ->
    Mlist.find_map (function t ->
      begin match t.desc with 
      | Trm_typedef ({typdef_typid = tid1;_} as td)  
        when tid = tid1 -> Some td
      | _ -> None
      end) tl
  | _ -> fail t_root.loc "typid_to_typedef: expected the ast of the main file"