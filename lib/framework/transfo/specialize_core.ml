open Prelude
open Target

(** [any_on e t]: replaces the function call [t] with [e]
      [e] - the expression replacing the call to function [ANY],
      [t] - ast of a call to function [ANY]. *)
let any_on (e : trm) (t : trm) : trm =
  let error = "Specialize_core.any_on: expected a trm_var with ANY annotation."  in
  let (f, _) = trm_inv ~error trm_apps_inv t in
  let any = trm_inv ~error trm_var_inv f in
  if Tools.pattern_matches "ANY?." any.name
    then  e
    else trm_fail f "Specialize_core.any_on: expected the special function ANY"

(** [choose_on selelct_arg t]: replaces the function call [t] with one of its arguments that satisfies
     the predicate  [select_arg],
      [select_arg] - a predicate on the index of the argument that should be choosed,
      [t] - ast of the call to function choose. *)
let choose_on (select_arg : var list -> int) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_f, argnb :: args, _)  ->
    begin match argnb.desc with
    | Trm_lit (Lit_int nb) ->
       if nb <> List.length args then trm_fail t "Specialize_core.choose_aux: number of args is not correct";
        let choices = List.map (fun arg ->
          match arg.desc with
          | Trm_var s -> s
          | Trm_apps (_, [v], _)  ->
            begin match v.desc with
            | Trm_var v -> v
            | _ -> trm_fail arg "Specialize_core.choose_aux: could not match non constant variable"
            end
          | _ ->
          trm_fail arg "Specialize_core.choose_aux: all the arguments of a
          function call should be variable occurrences\n and %s is not one \n") args  in
        let id = select_arg choices in
        if id < 0 || id > List.length choices -1 then trm_fail t "Specialize_core.choose_aux: select_arg function does not give a correct index";
        trm_var_get (List.nth choices id )
    | _ -> trm_fail argnb "Specialize_core.choose_aux: expected a literel trm"
    end
  | _ -> trm_fail t "Specialize_core.choose_on: expected a call to funtion choose"

(** [fun_def_aux spec_name spec_args t]: inserts a copy of the function definition [t], specializing
      one of its arguments based on the list [spec_args].
      [spec_name] - the name of the copy
      [spec_args] - an optional list of trms, telling the transformation which argss it shoudl specialize,
      [t] - ast of the function definition. *)
let fun_def_on (spec_name : string) (spec_args : (trm option) list) (t : trm) : trm =
  let spec_var = new_var spec_name in
  match t.desc with
  | Trm_let_fun (qf, ret_ty, args, body, _) ->
    (* Check if spec_args is of the correct shape. *)
    if List.length spec_args <> List.length args then trm_fail t "Specialize_core.fun_def_on: the list of arguments to specialize
        should match the list of the arguments of the targeted function.";

    let args_map = List.combine args spec_args in

    let new_args = List.fold_left (fun acc arg ->
      match List.assoc_opt arg args_map with
      | Some v ->
        begin match v with
        | Some _ -> acc
        | _ -> arg :: acc
        end
      | None -> assert false) [] (List.rev args) in

    let call_args = List.fold_left (fun acc (arg, ty) ->
      match List.assoc_opt (arg, ty) args_map with
      | Some v ->
        begin match v with
        | Some t1 -> t1 :: acc
        | None -> trm_var arg :: acc
        end
      | None -> assert false

    ) [] (List.rev args) in

    let new_body = trm_seq_nomarks [trm_apps (trm_var qf) call_args] in

    let new_def = trm_let_fun spec_var ret_ty new_args new_body in

    trm_seq_nobrace_nomarks [t; new_def]
  | _ -> trm_fail t "Specialize_core.fun_defs_aux: expected a target to a function definition."

(** [fun_call_on spec_name args_to_choose t]: replaces the function call [t] with another function call.
    [spec_name] - the name of the function that appears on the new call,
    [args_to_choose] - a list of booleans telling the transformation which of the current arguments
        from the current call should be kept.*)
let fun_call_on (spec_name : var) (args_to_choose : bool list) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_f, args, _) ->
    let new_args = List.fold_left2 (fun acc b t1 ->
      if b
        then t1 :: acc
        else acc) [] args_to_choose args in
    trm_apps (trm_var spec_name) (List.rev new_args)
  | _ -> trm_fail t "Specialize_core.fun_call_on: expected a target to a function call"
