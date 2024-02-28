open Ast
open Path
open Target

(* [Dep]: a module to represent inter-instruction data dependencies based on the
   [Ast.dep] data type. The role of [Ast.dep] is to represent dependencies
   between OpenMP tasks in the OpenMP [depend] directives. In APAC, we extend
   the use of [Ast.dep] to represent inter-instruction data dependencies in
   general, not necessarily in the OpenMP context. *)
module Dep : sig
  type t = dep
  type attr = Regular | FunArgIn | FunArgInOut
  val to_atomic : t -> t
  val to_string : t -> string
  val of_var : var -> int -> t
  val of_trm : trm -> var -> int -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t = dep
  (* [Dep.attr]: enumeration of dependency attributes. See
     [trm_discover_dependencies]. *)
  type attr =
    (* default value, no attribute *)
    | Regular
    (* function call argument, read-only dependency *)
    | FunArgIn
    (* function call argument, read-write dependency *)
    | FunArgInOut
  (* [Dep.to_atomic d]: if [d] is a dependency represented by a pointer
     expression, e.g. [*c], the function returns the base dependency element,
     i.e. the underlying variable [Dep_var] or term [Dep_trm]. If the degree of
     the pointer expression is greater than 1, it will recurse over the nested
     pointer expression until it reaches the underlying [Dep_var] or
     [Dep_trm]. *)
  let rec to_atomic (d : t) : t =
    match d with
    | Dep_ptr d' -> to_atomic d'
    | _ -> d
  (* [Dep.to_string d]: generates a string representation of the dependency [d].
     See also [Ast.dep]. *)
  let rec to_string (d : t) : string =
    match d with
    | Dep_ptr d' ->
       (*"*" ^*) to_string d'
    | Dep_var v -> v.name
    | Dep_trm (t, v) -> v.name (*AstC_to_c.ast_to_string t*)
  (* [Dep.of_var v degree]: converts the variable [v] into a dependency. If
     [degree] is greater than 0, the resulting [Dep_var] will be wrapped into
     [degree] pointer expressions [Dep_ptr]. *)
  let rec of_var (v : var) (degree : int) : t =
    (* For each level of the pointer degree *)
    let degree' = degree - 1 in
    if degree > 0 then
      (* nest a [Dep_ptr]. *)
      Dep_ptr (of_var v degree')
    else
      (* At the end, when the zero level is reached, nest the final [Dep_var]
         built from [v]. *)
      Dep_var (v)
  (* [Dep.of_trm t v degree]: converts the term [t] into a dependency. Note that
     a dependency term [Dep_trm] is composed of the term itself, e.g. [tab[0]],
     but also of the variable involved in the data access, e.g. [tab]. Moreover,
     if [degree] is greater than 0, the resulting [Dep_trm] will be wrapped into
     [degree] pointer expressions [Dep_ptr]. *)
  let rec of_trm (t : trm) (v : var) (degree : int) : t =
    (* For each level of the pointer degree *)
    let degree' = degree - 1 in
    if degree > 0 then
      (* nest a [Dep_ptr]. *)
      Dep_ptr (of_trm t v degree')
    else
      (* At the end, when the zero level is reached, nest the final [Dep_var]
         built from [v]. *)
      Dep_trm (t, v)
  (* [Dep.compare d1 d2]: compares two dependencies [d1] and [d2]. Note that
     dependencies are not only simple variables. We consider also pointer
     expressions, e.g. [*c], as well as more complex terms such as array
     accesses, e.g. [tab[0]]. Therefore, to compare two dependencies, we compare
     their string representation, see [Dep.to_string]. *)
  let compare (d1 : t) (d2 : t) : int =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    String.compare d1' d2'
  (* [Dep.equal d1 d2]: checks the equality of two dependencies [d1] and [d2].
     See also [Dep.compare]. *)
  let equal (d1 : t) (d2 : t) : bool =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    d1' = d2'
end

(* [Dep_set]: a module to represent sets of dependencies. Typically, we will use
   a set of input (read-only) dependencies as well as a set of input-output
   (read-write) dependencies. *)
module Dep_set = struct
  include Set.Make(Dep)
  (* [Dep_set.of_stack s]: converts the stack of dependencies [s] into a set of
     dependencies. *)
  let of_stack (s : Dep.t Stack.t) : t = of_seq (Stack.to_seq s)
  (* [Dep_set.to_list s]: converts the set of dependencies [s] into a list of
     dependencies. *)
  let to_list (s : t) = List.of_seq (to_seq s)
end

(* [find_parent_function p]: goes back up the path [p] and looks for the first
   term corresponding to a function definition. If a function definition is
   found, it returns the name of the function as a variable. We use
   [find_parent_function] to determine the parent function of a task group
   sequence in order to access its constification record in [const_funs]. *)
let find_parent_function (p : path) : var option =
  (* We shall go back on our steps in the path, i.e. in the direction of the
     root of the AST, so we need to reverse [p]. *)
  let reversed = List.tl (List.rev p) in
  (* We use an auxiliary function in order to hide to the outside world the need
     for the path reversal. *)
  let rec aux (p : path) : var option =
    (* The function simply goes recursively through the reversed [p] *)
    match p with
    | e :: f -> 
       begin
         (* and if it detects a function definition, it returns it. *)
         (* FIXME : Optimize by passing non-reversed list as argument ? *)
         let tg = target_of_path (List.rev p) in
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_let_fun (v, _, _, _, _) -> Some (v)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed 
