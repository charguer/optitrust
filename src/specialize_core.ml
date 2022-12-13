open Ast

(* [any_aux e t]: replaces the function call [t] with [e]
      [e] - the expression replacing the call to function [ANY],
      [t] - ast of a call to function [ANY]. *)
let any_aux (e : trm) (t : trm) : trm =
  let error = "Specialize_core.any_aux: expected a trm_var with ANY annotation."  in
  let (f, _) = trm_inv ~error trm_apps_inv t in
  let (_, any) = trm_inv ~error trm_var_inv f in
  if Tools.pattern_matches "ANY?." any 
    then  e
    else fail f.loc "Specialize_core.any_aux: expected the special function ANY"

(* [any e t p]: applies [any_aux] at trm [t] with path [p]. *)
let any (e : trm) : Target.Transfo.local =
  Target.apply_on_path (any_aux e)

(* [choose_aux  selelct_arg t]: replaces the function call [t] with one of its arguments that satisfies
     the predicate  [select_arg],
      [select_arg] - a predicate on the index of the argument that should be choosed,
      [t] - ast of the call to function choose. *)
let choose_aux (select_arg : string list -> int) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_f, argnb :: args)  ->
    begin match argnb.desc with
    | Trm_val (Val_lit (Lit_int nb)) ->
       if nb <> List.length args then fail t.loc "Specialize_core.choose_aux: number of args is not correct";
        let choices = List.map (fun arg ->
          match arg.desc with
          | Trm_var (_, s) -> s.qvar_var
          | Trm_apps (_, [v])  ->
            begin match v.desc with
            | Trm_var (_, v) -> v.qvar_var
            | _ -> fail arg.loc "Specialize_core.choose_aux: could not match non constant variable"
            end
          | _ ->
          fail arg.loc "Specialize_core.choose_aux: all the arguments of a
          function call should be variable occurrences\n and %s is not one \n") args  in
        let id = select_arg choices in
        if id < 0 || id > List.length choices -1 then fail t.loc "Specialize_core.choose_aux: select_arg function does not give a correct index";
        trm_var_get (List.nth choices id )
    | _ -> fail argnb.loc "Specialize_core.choose_aux: expected a literel trm"
    end
  | _ -> fail t.loc "Specialize_core.choose_aux: expected a call to funtion choose"


(* [choose select_arg t p]: applies [choose_aux] at trm [t] with path [p]. *)
let choose (select_arg : string list -> int) : Target.Transfo.local =
  Target.apply_on_path (choose_aux select_arg)
