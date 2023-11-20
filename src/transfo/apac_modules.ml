open Ast

(* [lvar]: labelled variable type. We make use of this type to make a difference
   between class member variables. Indeed, in the case of a class member, the
   associated variable is represented always by [this]. As a result every member
   of a given class ends up with [this] as name and the same identifier (which
   changes only from one class method to another).

   For example, let us consider:

   class C {
     int a; int b;
     int f() { int c = a / b; return c; }
     int g() { return a * b; }
   }

   In [f], both [a] and [b] will have [this] as name as well as the same
   identifier, 75 for example. In [g], the identifier will change, let us say to
   42, but then both [a] and [b] in [g] will have [this] as name and 42 as
   identifier. The only way to make a difference between two class memebers
   within a member method is to look at the labels (strings) associated with
   each occurence. To take into account this situation within the constification
   process, we use a variable type extended with a label.
*)
type lvar = { v : var; l : label; }

(* [lvar_to_string] returns a string representation of the labelled variable
   [lv]. *)
let lvar_to_string (lv : lvar) : string =
  let q_str = String.concat "" (List.map (fun q -> q ^ "::") lv.v.qualifier) in
  let id_str = if lv.v.id = -1 then "?" else (string_of_int lv.v.id) in
  let member = if lv.l <> "" then lv.l ^ "#" else lv.l in
  q_str ^ lv.v.name ^ "#" ^ member ^ id_str

(* [LVar] and [LVar_Hashtbl]: specific type of hash tables where the keys are of
   type [lvar]. *)
module LVar = struct
  type t = lvar
  let equal lv1 lv2 = var_eq lv1.v lv2.v && lv1.l = lv2.l
  let hash lv = Hashtbl.hash ((string_of_int lv.v.id) ^ lv.l)
end

module LVar_Hashtbl = Hashtbl.Make(LVar)

let rec dep_to_string (d : dep) : string =
  match d with
  | Dep_ptr d' ->
     "*" ^ (dep_to_string d')
  | Dep_var v -> v.name
  | Dep_trm (t, _) ->
     AstC_to_c.ast_to_string t

let rec dep_get_atomic (d : dep) : dep =
  match d with
  | Dep_ptr d' -> dep_get_atomic d'
  | _ -> d

module Dep = struct
  type t = dep
  let compare d1 d2 =
    let d1' = dep_to_string d1 in
    let d2' = dep_to_string d2 in
    String.compare d1' d2'
  let equal d1 d2 =
    let d1' = dep_to_string d1 in
    let d2' = dep_to_string d2 in
    d1' = d2'
end

module Dep_set = Set.Make(Dep)

let dep_set_of_stack s = Dep_set.of_seq (Stack.to_seq s)

let var_set_of_var_hashtbl h = Var_set.of_seq (Var_Hashtbl.to_seq_keys h)

(* [typed_var_to_dep tv]: converts the typed variable [tv] into a data
   dependency (see the [Ast.dep] data type). *)
let rec var_to_dep (v : var) (degree : int) : dep =
  (* For each level of the pointer degree *)
  let degree' = degree - 1 in
  if degree > 0 then
    (* nest a [Dep_ptr]. *)
    Dep_ptr (var_to_dep v degree')
  else
    (* At the end, when the zero level is reached, nest the final [Dep_var]
       built from [v]. *)
    Dep_var (v)

(* [typed_var_to_dep tv]: converts the typed variable [tv] into a data
   dependency (see the [Ast.dep] data type). *)
let rec trm_to_dep (t : trm) (v : var) (degree : int) : dep =
  (* For each level of the pointer degree *)
  let degree' = degree - 1 in
  if degree > 0 then
    (* nest a [Dep_ptr]. *)
    Dep_ptr (trm_to_dep t v degree')
  else
    (* At the end, when the zero level is reached, nest the final [Dep_var]
       built from [v]. *)
    Dep_trm (t, v)
