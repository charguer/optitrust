open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [any_aux var t]: replace a function call t with a variable occurrence [var]
      params:
        [var]: the variablee replacing the function call [t]
        [t]: ast of a call to function [ANY]
      return:
        the ast of [var]
*)
let any_aux (array_index : var) (t : trm) : trm =
  match t.desc with 
  | Trm_apps (f,_) ->
    begin match f.desc with
    | Trm_var (_, any) when Tools.pattern_matches "ANY?." any ->  trm_var array_index
    | _ -> fail f.loc "any_aux: expected the special function ANY"
    end
  | _ -> fail t.loc "any_aux: expected a trm_var with ANY annotation"

let any (array_index : var) : Target.Transfo.local =
  Target.apply_on_path (any_aux array_index)

(* [choose_aux  selelct_arg t]: replace the function call t with one of its arguments which statisfies
        the predicate select_arg
      params:
        [select_arg]: a predicate on the index of the argument which should be choosed
        [t]: ast of the call to function choose
      return:
        the variable occurrence of the selected argument
*)
let choose_aux (select_arg : string list -> int) (t : trm) : trm =
  match t.desc with 
  | Trm_apps (_f, argnb :: args)  -> 
    begin match argnb.desc with 
    | Trm_val (Val_lit (Lit_int nb)) -> 
       if nb <> List.length args then fail t.loc "choose_aux: number of args is not correct";
        let choices = List.map (fun arg -> 
          match arg.desc with 
          | Trm_var (_, s) -> s 
          | Trm_apps (_, [v])  -> 
            begin match v.desc with 
            | Trm_var (_, v) -> v 
            | _ -> fail arg.loc "choose_aux: could not match non constant variable"
            end
          | _ ->  
          fail arg.loc "choose_aux: all the arguments of a 
          function call should be variable occurrences\n and %s is not one \n") args  in
        let id = select_arg choices in
        if id < 0 || id > List.length choices -1 then fail t.loc "choose_aux: select_arg function does not give a correct index";
        trm_var_get (List.nth choices id ) 
    | _ -> fail argnb.loc "choose_aux: expected a literel trm"
    end
   
  | _ -> fail t.loc "choose_aux: expected a call to funtion choose"

let choose (select_arg : string list -> int) : Target.Transfo.local =
  Target.apply_on_path (choose_aux select_arg)