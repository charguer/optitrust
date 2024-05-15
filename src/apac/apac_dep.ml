open Ast
open Path
open Target

(** [DepAttr]: a module to represent dependency attributes. See [Dep]. *)
module DepAttr : sig
  type t =
    ArgIn | ArgInOut | InductionVariable | Condition | Subscripted | Accessor
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end = struct
  type t =
    (* read-only argument *)
    | ArgIn
    (* read-write argument *)
    | ArgInOut
    (* induction variable in a loop *)
    | InductionVariable
    (* part of a conditionnal statement, e.g. in an if-statement or in a
       switch-statement *)
    | Condition
    (* subscripted array access *)
    | Subscripted
    (* non-constant element of an array accessor, e.g. `i' in `tab[i + 1]' or
       `idx[i]' and `i' in `tab[idx[i]]' *)
    | Accessor
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
    | ArgIn -> "ArgIn"
    | ArgInOut -> "ArgInOut"
    | InductionVariable -> "InductionVariable"
    | Condition -> "Condition"
    | Subscripted -> "Subscripted"
    | Accessor -> "Accessor"
end

(** [DepAttr_set]: a module to represent sets of dependency attributes. *)
module DepAttr_set = struct
  include Set.Make(DepAttr)
  (** [DepAttr_set.to_string das]: returns a string representation of the set of
      dependency attributes [das]. *)
  let to_string (das : t) : string =
    fold (fun da acc -> acc ^ (DepAttr.to_string da) ^ " ") das " "
end

(** [Dep]: a module to represent inter-instruction data dependencies based on
    the [Ast.dep] data type. The role of [Ast.dep] is to represent dependencies
    between OpenMP tasks in the OpenMP [depend] directives. In APAC, we extend
    the use of [Ast.dep] to represent inter-instruction data dependencies in
    general, not necessarily in the OpenMP context. *)
module Dep : sig
  type t = dep
  val degree : t -> int
  val of_trm : trm -> var -> int -> t
  val to_string : t -> string
  val to_string2 : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val equal2 : t -> t -> bool
  val hash : t -> int
end = struct
  type t = dep
  (** [Dep.degree d]: returns the degree of the dependency [d] if it is of type
      [Dep_trm]. In this case, the degree denotes the pointer degree or the
      number of array dimensions of the [var] being part of [d].

      For example, if [d] is '(tab\[0\]\[1\], tab)', the function returns 2. *)
  let degree (d : t) : int =
    match d with
    | Dep_trm (a, v) ->
       (* Get nested accesses of [a], the term associated with the dependency,
          and count them. This represents the degree of [d]. *)
       let (_, al) = get_nested_accesses a in List.length al
    (* If [d] is not of type [Dep_trm], the degree is null as it is not an array
       access. *)
    | _ -> 0
  (** [Dep.of_trm vt v degree]: converts the variable term [vt] and the
      underlying variable [v] into a dependency of type [Dep_trm]. This
      dependency type is meant for pointers and array accesses. Therefore,
      besides the variable [v] behind [vt], a [Dep_trm] features also an
      adequate term for accessing the first element of [vt].

      For example, if [vt], and consequently [v], are 'tab' and [degree] is 2,
      the function produces a dependency term [Dep_trm] composed of
      'tab\[0\]\[0\]' and 'tab'. *)
  let rec of_trm (vt : trm) (v : var) (degree : int) : t =
    if degree < 1 then
      Dep_trm (vt, v)
    else
      let v0 = Val_lit (Lit_int 0) in
      let v0 = Trm.trm_val v0 in
      of_trm (Trm.trm_array_get vt v0) v (degree - 1)
  (** [Dep.to_string d]: generates a string representation of the dependency
      [d]. See also [Ast.dep]. *)
  let rec to_string (d : t) : string =
    match d with
    | Dep_var v -> v.name
    | Dep_trm (t, _) -> AstC_to_c.ast_to_string t
    | Dep_ptr d' -> to_string d'
  (** [Dep.to_string2 d]: generates a string representation of the dependency
      [d]. Unlike [Dep.to_string], this function considers only the variable
      part of [Dep.t] and the pointer degree if [d] is a [Dep_trm]. In this
      case, it generates the string representation of the second member of the
      ([trm], [var]) pair appended with `n' pairs of brackets '[]' where `n' is
      the pointer degree of [var]. For example, [Dep.to_string2] of
      'tab\[i\]\[2\]' gives 'tab\[\]\[\]'. *)
  let rec to_string2 (d : t) : string =
    match d with
    | Dep_var v -> v.name
    | Dep_trm (t', v) ->
       let (_, accesses) = get_nested_accesses t' in
       let accesses = List.fold_left (fun acc _ -> "[]" ^ acc) "" accesses in
       v.name ^ accesses
    | Dep_ptr d' -> to_string2 d'
  (** [Dep.compare d1 d2]: compares two dependencies [d1] and [d2]. Note that
      dependencies are not only simple variables. We consider also pointer
      expressions, e.g. '*c', as well as more complex terms such as array
      accesses, e.g. 'tab[0]'. Therefore, to compare two dependencies, we
      compare their string representation, see [Dep.to_string]. *)
  let compare (d1 : t) (d2 : t) : int =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    String.compare d1' d2'
  (** [Dep.equal d1 d2]: checks the equality of two dependencies [d1] and
      [d2]. *)
  let equal (d1 : t) (d2 : t) : bool =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    d1' = d2'
  (** [Dep.equal2 d1 d2]: checks the equality of two dependencies [d1] and [d2]
      based on the string representation produced with [Dep.to_string2]. *)
  let equal2 (d1 : t) (d2 : t) : bool =
    let d1' = to_string2 d1 in
    let d2' = to_string2 d2 in
    d1' = d2'
  let hash (d : t) : int =
    let d' = to_string d in
    Hashtbl.hash d'
end

(** [Dep_set]: a module to represent sets of dependencies. Typically, we will
    use a set of input (read-only) dependencies as well as a set of input-output
    (read-write) dependencies. *)
module Dep_set = struct
  include Set.Make(Dep)
  (** [Dep_set.inter2]: computes the intersection of [s1] and [s2] while using
      the alternative [Dep.equal2] equality check. *)
  let inter2 (s1 : t) (s2 : t) : t =
    fold (fun d res ->
        let check = exists (fun d' -> Dep.equal2 d' d) s2 in
        if check then add d res else res) s1 empty
  (** [Dep_set.of_stack s]: converts the stack of dependencies [s] into a set of
      dependencies. *)
  let of_stack (s : Dep.t Stack.t) : t = of_seq (Stack.to_seq s)
  (** [Dep_set.to_list s]: converts the set of dependencies [s] into a list of
      dependencies. *)
  let to_list (s : t) = List.of_seq (to_seq s)
  (** [Dep_set.to_string s]: generates a string representation of the set of
      dependencies [s]. *)
  let to_string (s : t) : string =
    if is_empty s then "[ empty ]"
    else "[" ^ (fold (fun d res -> res ^ (Dep.to_string d) ^ " ") s " ") ^ "]"
end

(** [Dep_map]: a module to represent maps of dependencies. *)
module Dep_map = struct
  include Map.Make(Dep)
  (** [Dep_map.union2 dm1 dm2]: computes the union of the maps of dependencies
      to sets of dependency attributes [dm1] and [dm2]. *)
  let union2 (dm1 : DepAttr_set.t t) (dm2 : DepAttr_set.t t) : DepAttr_set.t t =
    union (fun _ v1 v2 ->
        let v = DepAttr_set.union v1 v2 in Some v
      ) dm1 dm2
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
  (** [Dep_map.may_bind_set ds das dm f]: does the same thing as
      [Dep_map.bind_set ds das dm], but only for those dependencies from [ds]
      for which the function [f] returns [true]. *)
  let may_bind_set (ds : Dep_set.t) (das : DepAttr_set.t)
        (dm : DepAttr_set.t t) (f : (Dep.t -> bool)) : DepAttr_set.t t =
    Dep_set.fold (fun d acc ->
        if (f d) then
          let b = find_opt d dm in
          match b with
          | Some das' ->
             (* If a previous binding exists, we compute the union of the sets
                of dependency attributes. *)
             let das'' = DepAttr_set.union das das' in
             add d das'' acc
          | None ->
             (* Otherwise, we create a new binding. *)
             add d das acc
        else acc) ds dm
  (** [Dep_map.has_with_attribute d da dm]: checks whether the map of
      dependencies to sets of dependency attributes [dm] has a binding for the
      dependency [d] with the dependency attribute [da] in its set of dependency
      attributes. *)
  let has_with_attribute (d : Dep.t) (da : DepAttr.t)
        (dm : DepAttr_set.t t) : bool =
    exists (fun k a -> Dep.equal k d && DepAttr_set.mem da a) dm
  (** [Dep_map.remove_attribute da dm]: removes the dependency attribute [da]
      from all the bindings in the map of dependencies to dependency attribute
      sets [dm]. *)
  let remove_attribute
        (da : DepAttr.t) (dm : DepAttr_set.t t) : DepAttr_set.t t =
    map (fun das -> DepAttr_set.remove da das) dm        
  (** [Dep_map.of_stack s]: converts the stack of dependency-dependency
      attribute set pairs [s] to a map of dependencies to dependency attribute
      sets. *)
  let of_stack (s : (dep * DepAttr_set.t) Stack.t) : DepAttr_set.t t =
    Stack.fold (fun res (d, das) -> add d das res) empty s
  (** [Dep_map.to_string dm]: generates a string representation of the map of
      dependencies to sets of dependency attributes [dm]. *)
  let to_string (dm : DepAttr_set.t t) : string =
    if is_empty dm then "[ empty ]"
    else "[" ^
           (fold (fun d das res ->
                res ^ (Dep.to_string d) ^ "(" ^ (DepAttr_set.to_string das) ^
                  ") ") dm " "
           ) ^ "]"
end

(** [ioattrs_map]: a type of maps of dependencies to sets of dependency
    attributes. *)
type ioattrs_map = DepAttr_set.t Dep_map.t

(** [Dep_hashtbl]: a module to represent hash tables of dependencies. *)
module Dep_hashtbl = Hashtbl.Make(Dep)

(** [find_parent_function p]: goes back up the path [p] and looks for the first
    term corresponding to a function definition. If a function definition is
    found, it returns the name of the function as a variable. We use
    [find_parent_function] to determine the parent function of a task group
    sequence in order to access its constification record in [const_funs]. *)
let find_parent_function (p : path) : var option =
  (** We shall go back on our steps in the path, i.e. in the direction of the
      root of the AST, so we need to reverse [p]. *)
  let reversed = List.tl (List.rev p) in
  (** We use an auxiliary function in order to hide to the outside world the
      need for the path reversal. *)
  let rec aux (p : path) : var option =
    (** The function simply goes recursively through the reversed [p] *)
    match p with
    | e :: f -> 
       begin
         (** and if it detects a function definition, it returns it. *)
         (** FIXME : Optimize by passing non-reversed list as argument ? *)
         let tg = target_of_path (List.rev p) in
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_let_fun (v, _, _, _, _) -> Some (v)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed 
