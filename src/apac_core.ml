open Ast
open Target


(* [use_goto_for_return_aux mark t]: transforms the body of the funciton declaration in such a way that
      all return statements are replaced with gotos,
    [mark] - mark used to mark the introduced sequence.
    [t] - ast of the function definition. *)
let use_goto_for_return_aux (mark : mark) (t : trm) : trm =
  match t.desc with 
  | Trm_let_fun (qn, ret_ty, args, body) -> 
    let seq_to_insert, _ = Internal.replace_return_with_assign ~check_terminal:false ~exit_label:"__exit" "__res" body in 
    let new_body = 
      begin match ret_ty.typ_desc with 
      | Typ_unit -> 
        trm_add_mark mark (trm_seq_nomarks [
          seq_to_insert;
          trm_add_label "__exit" (trm_unit())])
      | _ -> 
        let new_decl = trm_let_mut ("__res", ret_ty) (trm_uninitialized ()) in 
        trm_add_mark mark (trm_seq_nomarks [
          new_decl;
          seq_to_insert;
          trm_add_label "__exit" (trm_unit());
          trm_ret (Some (trm_var_get "__res"))
        ])
      end in 
      trm_alter ~desc:(Some (Trm_let_fun (qn, ret_ty, args, new_body))) t
  | _ -> fail t.loc "Apac_core.use_goto_for_return_aux: expected  a target to a function definition."


(* [use_goto_for_return mark t p]: applies [use_goto_for_return_aux] at the trm [t] with path [p]. *)
let use_goto_for_return (mark : mark) (t : trm) (p : path) : trm =
  apply_on_path(use_goto_for_return_aux mark) t p


(* [dep_kind]: type used for [arg_dep]. *)
type dep_kind = 
  | Dep_kind_in 
  | Dep_kind_out
  | Dep_kind_inout
  | Dep_kind_outin
  | Dep_kind_sink
  | Dep_kind_source

(* [arg_dep]: a record that stored all the dependecy informations about one argument. *)
type arg_dep = {
  arg_dep_var : var;
  arg_dep_typ : typ;
  arg_dep_kind : dep_kind;
}

(* [arg_deps]: a list of arg_dep. *)
type arg_deps = arg_dep list


(* [sorted_arg_deps]: a record used for storing classified arguments based on their respective dependency kind. *)
type sorted_arg_deps = {
  dep_in : deps;
  dep_out : deps;
  dep_inout : deps;
  dep_outin : deps;
  (* LATER: add other fields. *)
}


(* [get_arg_dependencies t]: for each argument of the function [t] returns all the dependencies.  *)
let get_arg_dependencies (t : trm) : arg_deps =
  match Function_core.get_function_prototype t with 
  | Some (ty, args) -> 
    List.map (fun (x, ty) -> 
      { arg_dep_var = x;
        arg_dep_typ = ty;
        arg_dep_kind = Dep_kind_in (* TODO: Michel, dep_kind_of_typ  *)}
     ) args
  | None -> fail t.loc "Ast.get_arg_dependencies: expected a function definition"


(* TODO: Michel
    [sort_arg_dependencies arg_deps]: classifies all the argument dependencies into four categories.
    In, Out, Inout, Outin. *)
let sort_arg (arg_deps : arg_deps) : sorted_arg_deps = 
 { dep_in = [];
   dep_out = [];
   dep_inout = [];
   dep_outin = [];}
