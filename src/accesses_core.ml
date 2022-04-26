open Ast

(* [transform_aux f_get f_set t]: apply f_get or f_set depending on the fact if 
    [t] is a get operation or a set operation
   params:
    [f_get]: the get operation that is going to be applied 
    [f_set]: the set operation that is going to be applied
    [t]: the ast of the node where the operation is applied to *)
let transform_aux (f_get : trm -> trm) (f_set : trm -> trm) (t : trm) : trm = 
  match t.desc with 
  | Trm_apps (_, [_addr]) -> 
    if is_get_operation t 
      then f_get t 
      else fail t.loc "transform_aux: expected a get operation"
  | Trm_apps (f, [addr; targ]) ->
      if is_set_operation t
        then {t with desc = Trm_apps (f ,[addr; f_set targ])}
        else fail t.loc "Accesses_core.transform_aux: expected a set operation"
  | _ -> fail t.loc "Accessses_core.transform_aux: expected either a get or set operation"


(* [transform f_get f_set t p]: applies [transform_aux] at the trm [t] with path [p] *)
let transform (f_get : trm -> trm) (f_set : trm -> trm) : Target.Transfo.local = 
  Target.apply_on_path (transform_aux f_get f_set)

(* [intro_aux t]: changes encodings "struct_get(get (t), f)" to "get(struct_access (t, f))"
    params:
      [t]: ast of the node where the accesses can be found *)
let intro_aux (t : trm) : trm = 
  let rec aux (t : trm) : trm = 
    match t.desc with 
    | Trm_apps (f, [arg]) -> 
      begin match trm_prim_inv f with 
      | Some (Prim_unop (Unop_struct_get x)) -> 
        begin match arg.desc with 
        | Trm_apps (_, [arg1]) when is_get_operation arg -> 
          trm_get ~annot:arg.annot (trm_apps (trm_unop (Unop_struct_access x)) [arg1])
        | _ -> t
        end 
      | _ -> trm_map aux t 
      end 
    | _ -> trm_map aux t
  in 
  aux t

let intro : Target.Transfo.local = 
  Target.apply_on_path (intro_aux)