open Ast
open Path
open Target

(** [DepAttr]: a module to represent dependency attributes. See [Dep]. *)
module DepAttr : sig
  type t = Regular | FunArgIn | FunArgInOut | InductionVariable | Condition
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end = struct
  type t =
    (* default value, no attribute *)
    | Regular
    (* function call argument, read-only dependency *)
    | FunArgIn
    (* function call argument, read-write dependency *)
    | FunArgInOut
    (* induction variable in a loop *)
    | InductionVariable
    (* part of a conditionnal statement, e.g. in an if-statement or in a
       switch-statement *)
    | Condition
  (** [DepAttr.compare da1 da2]: compares two dependency attributes [da1] and
      [da2]. As [DepAttr.t] is an enumeration type, this is a simple comparison
      between two integer values representing the associated enumeration labels
      being compared. *)
  let compare (da1 : t) (da2 : t) : int =
    if da1 = da2 then 0
    else if da1 > da2 then 1
    else -1
  (** [DepAttr.equal da1 da2]: checks the equality of two task attributes [da1]
      and [da2]. See [DepAttr.compare]. *)
  let equal (da1 : t) (da2 : t) : bool = compare da1 da2 = 0
  (** [DepAttr.to_string da]: returns a string representation of the dependency
      attribute [da]. *)
  let to_string (da : t) : string =
    match da with
    | Regular -> "Regular"
    | FunArgIn -> "FunArgIn"
    | FunArgInOut -> "FunArgInOut"
    | InductionVariable -> "InductionVariable"
    | Condition -> "Condition"
end

(** [DepAttr_set]: a module to represent sets of dependency attributes. *)
module DepAttr_set = struct
  include Set.Make(DepAttr)
  (** [DepAttr_set.to_string das]: returns a string representation of the set of
      dependency attributes [das]. *)
  let to_string (das : t) : string =
    fold (fun da acc -> acc ^ (DepAttr.to_string da) ^ " | ") das " | "
end

(** [Dep]: a module to represent inter-instruction data dependencies based on
    the [Ast.dep] data type. The role of [Ast.dep] is to represent dependencies
    between OpenMP tasks in the OpenMP [depend] directives. In APAC, we extend
    the use of [Ast.dep] to represent inter-instruction data dependencies in
    general, not necessarily in the OpenMP context. *)
module Dep : sig
  type t = dep
  val degree : t -> int
  val to_atomic : t -> t
  val of_atomic : t -> int -> t
  val to_string : t -> string
  val of_var : var -> int -> t
  val of_trm : trm -> var -> int -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t = dep
  (** [Dep.degree d]: returns the degree of the dependency [d], i.e. the number
      of enclosing [Dep_ptr]s. *)
  let rec degree (d : t) : int =
    match d with
    | Dep_ptr d' -> (degree d') + 1
    | _ -> 0
  (** [Dep.to_atomic d]: if [d] is a dependency represented by a pointer
      expression, e.g. '*c', the function returns the base dependency element,
      i.e. the underlying variable [Dep_var] or term [Dep_trm]. If the degree of
      the pointer expression is greater than 1, it will recurse over the nested
      pointer expression until it reaches the underlying [Dep_var] or
      [Dep_trm]. *)
  let rec to_atomic (d : t) : t =
    match d with
    | Dep_ptr d' -> to_atomic d'
    | _ -> d
  (** [Dep.of_atomic d degree]: converts the dependency [d] into a composed
      dependency. If [degree] is greater than 0, the [d] will be wrapped into
      [degree] pointer expressions [Dep_ptr]. *)
  let rec of_atomic (d : t) (degree : int) : t =
    (* For each level of the pointer degree *)
    let degree' = degree - 1 in
    if degree > 0 then
      (* nest a [Dep_ptr]. *)
      Dep_ptr (of_atomic d degree')
    else
      (* At the end, when the zero level is reached, return the initial [d]. *)
      d
  (** [Dep.to_string d]: generates a string representation of the dependency
      [d]. See also [Ast.dep]. *)
  let rec to_string (d : t) : string =
    match d with
    | Dep_ptr d' ->
       (*"*" ^*) to_string d'
    | Dep_var v -> v.name
    | Dep_trm (t, v) -> v.name (*AstC_to_c.ast_to_string t*)
  (** [Dep.of_var v degree]: converts the variable [v] into a dependency. If
      [degree] is greater than 0, the resulting [Dep_var] will be wrapped into
      [degree] pointer expressions [Dep_ptr]. *)
  let of_var (v : var) (degree : int) : t =
    (* Build a [Dep_var] from [v] and wrap it into pointer expressions, if
       necessary. *)
    let d = Dep_var (v) in of_atomic d degree
  (** [Dep.of_trm t v degree]: converts the term [t] into a dependency. Note
      that a dependency term [Dep_trm] is composed of the term itself, e.g.
      'tab\[0\]', but also of the variable involved in the data access, e.g.
      'tab'. Moreover, if [degree] is greater than 0, the resulting [Dep_trm]
      will be wrapped into [degree] pointer expressions [Dep_ptr]. *)
  let of_trm (t : trm) (v : var) (degree : int) : t =
    (* Build a [Dep_trm] from [t] and [v] and wrap it into pointer expressions,
       if necessary. *)
    let d = Dep_trm (t, v) in of_atomic d degree
  (** [Dep.compare d1 d2]: compares two dependencies [d1] and [d2]. Note that
      dependencies are not only simple variables. We consider also pointer
      expressions, e.g. '*c', as well as more complex terms such as array
      accesses, e.g. 'tab[0]'. Therefore, to compare two dependencies, we
      compare their string representation, see [Dep.to_string]. *)
  let compare (d1 : t) (d2 : t) : int =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    String.compare d1' d2'
  (** [Dep.equal d1 d2]: checks the equality of two dependencies [d1] and [d2].
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

(** [Dep_map]: a module to represent maps of dependencies. *)
module Dep_map = struct
  include Map.Make(Dep)
  (** [Dep_map.bind_set ds das dm]: updates the bindings of the dependencies
      from the dependency set [ds] in the map of dependencies to sets of
      dependency attributes [dm] with the set of dependency attributes [das]. *)
  let bind_set (ds : Dep_set.t) (das : DepAttr_set.t)
        (dm : DepAttr_set.t t) : DepAttr_set.t t =
    Dep_set.fold (fun d acc ->
        let b = find_opt d dm in
        match b with
        | Some das' ->
           (* If a previous binding exists, we compute the union of the sets of
              dependency attributes. *)
           let das'' = DepAttr_set.union das das' in
           add d das'' acc
        | None ->
           (* Otherwise, we create a new binding. *)
           add d das acc
      ) ds dm
  (** [Dep_map.has_with_attribute d da dm]: checks whether the map of
      dependencies to sets of dependency attributes [dm] has a binding for the
      dependency [d] with the dependency attribute [da] in its set of dependency
      attributes. *)
  let has_with_attribute (d : Dep.t) (da : DepAttr.t)
        (dm : DepAttr_set.t t) : bool =
    exists (fun k a -> Dep.equal k d && DepAttr_set.mem da a) dm
end

(** [ioattrs_map]: a type of maps of dependencies to sets of dependency
    attributes. *)
type ioattrs_map = DepAttr_set.t Dep_map.t

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
