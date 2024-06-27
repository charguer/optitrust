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
    (** read-only argument *)
    | ArgIn
    (** read-write argument *)
    | ArgInOut
    (** induction variable in a loop *)
    | InductionVariable
    (** part of a conditionnal statement, e.g. in an if-statement or in a
        switch-statement *)
    | Condition
    (** subscripted array access *)
    | Subscripted
    (** non-constant element of an array accessor, e.g. `i' in `tab\[i + 1\]' or
       `idx\[i\]' and `i' in `tab\[idx\[i\]\]' *)
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
  type subscripted_mode = ElementWise | Dimension | Variable
  val degree : t -> int
  val is_trm : t -> bool
  val of_trm : trm -> var -> int -> t
  val of_degree : trm -> var -> int -> t list
  val of_array : trm -> var -> t list
  val to_string : ?mode:subscripted_mode -> t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val equal_dimension : t -> t -> bool
  val equal_variable : t -> t -> bool
  val hash : t -> int
end = struct
  type t = dep

  (** Mode of interpretation of dependencies on subscripted accesses. *)
  type subscripted_mode =
    (** interpreted as-is *)
    | ElementWise
    (** entire dimensions carry the dependency *)
    | Dimension
    (** entire array carries the dependency *)
    | Variable
  
  (** [Dep.degree d]: returns the degree of the dependency [d] if it is of type
      [Dep_trm]. In this case, the degree denotes the pointer degree or the
      number of array dimensions of the [var] being part of [d].

      For example, if [d] is '(tab\[0\]\[1\], tab)', the function returns 2. *)
  let degree (d : t) : int =
    match d with
    (** If [d] has a term representation, get nested accesses of [a], the term
        associated with the dependency, and count them. This represents the
        degree of [d]. *)
    | Dep_trm (a, v) -> let (_, al) = get_nested_accesses a in List.length al
    (** If [d] is a direct variable access, the degree is null. *)
    | Dep_var _ -> 0
    (** Fail in all other cases. *)
    | Dep_ptr _ -> failwith "Dep.to_string: Unsupported dependency type."

  (** [Dep.is_trm d]: checks whether the dependency [d] is of type [Dep_trm]. *)
  let is_trm (d : t) : bool =
    match d with
    | Dep_trm _ -> true
    | _ -> false
  
  (** [Dep.of_trm t v degree]: converts the term [t] and the underlying variable
      [v] into a dependency of type [Dep_trm]. This dependency type is meant for
      subscripted accesses. Therefore, besides the variable [v] behind [t], a
      [Dep_trm] features also an adequate term for accessing the first element
      of [v].

      For example, if [t] as well as [v] are 'tab' and [degree] is 2, the
      function produces ('tab\[0\]\[0\]', 'tab'). Moreover, if [t] is
      'tab\[i\]', [v] is 'tab' and [degree] is 1, the function produces
      ('tab\[i\]\[0\]', 'tab'). *)
  let rec of_trm (t : trm) (v : var) (degree : int) : t =
    if degree < 1 then
      Dep_trm (t, v)
    else
      let v0 = Val_lit (Lit_int 0) in
      let v0 = Trm.trm_val v0 in
      of_trm (Trm.trm_array_get t v0) v (degree - 1)
  
  (** [Dep.of_degree t v degree]: calls [Dep.of_trm vt v degree] for each value
      of [degree] greather than 0 and puts the results of these calls into a
      list appended with [Dep_var v]. *)
  let rec of_degree (t : trm) (v : var) (degree : int) : t list =
    if degree < 1 then
      [Dep_var v]
    else
      (of_trm t v degree) :: (of_degree t v (degree - 1))

  (** [Dep.of_array t v]: based on the subscripted access term [t], it generates
      a list of [Dep_trm] dependencies where the first dependency is on the
      initially accessed element in [t] and the others are dependencies on
      potential parent elements in the case of a multi-dimensional array.

      For example, if [t] is 'tab\[i\]\[0\]' and [v] is 'tab', the function
      produces a list containing ('tab\[i\]\[0\]', 'tab\[i\]', 'tab'). *)
  let of_array (t : trm) (v : var) : t list =
    let rec accesses (t : trm) : trms =
      match t.desc with
      | Trm_apps ({desc = Trm_val
                            (Val_prim
                               (Prim_binop Binop_array_access)); _}, [t'; i]) ->
         t :: (accesses t')
      | Trm_var _ -> [t]
      | _ -> failwith "Dep.of_array: Inconsistent array access."
    in
    let a = accesses t in
    List.map (fun e -> Dep_trm (e, v)) a
  
  (** [Dep.to_string ?mode d]: returns a string form of the dependency [d].

      If [mode] id set to [ElementWise], we consider that dependencies
      represented by subscripted accesses, i.e. dependencies of type [Dep_trm],
      are carried only by the element specified by the associated index array
      operator expression, within the first [trm] component of [Dep_trm]. In
      this case, the function generates the string representation of the [trm]
      member of [Dep_trm].
      
      If [mode] is set to [Dimension], we consider that dependencies represented
      by subscripted accesses, i.e. dependencies of type [Dep_trm], are carried
      by the entire array dimension and not only the element specified by the
      associated index array operator expression, within the first [trm]
      component of [Dep_trm]. In this case, the function generates the string
      representation of the [var] member of [Dep_trm] appended with [n] pairs of
      brackets '\[\]' where [n] is the pointer degree of [var].

      If [strict] is set to [Variable], we consider that dependencies
      represented by subscripted accesses, i.e. dependencies of type [Dep_trm],
      are carried by the entire array. In this case, the function generates the
      string representation of the [var] member of [Dep_trm].

      For example:
      - [Dep.to_string] or [Dep.to_string ~mode:ElementWise] of 'tab\[i\]\[2\]'
        gives 'tab\[i\]\[2\]';
      - [Dep.to_string ~mode:Dimension] of 'tab\[i\]\[2\]' gives 'tab\[\]\[\]';
      - [Dep.to_string ~strict:Variable] of 'tab\[i\]\[2\]' gives 'tab'.
   *)
  let to_string ?(mode : subscripted_mode = ElementWise) (d : t) : string =
    match d with
    | Dep_var v -> v.name
    | Dep_trm (_, v) when mode = Variable -> v.name
    | Dep_trm (_, v) when mode = Dimension ->
       let rec make (n : int) : string =
         if n > 1 then "[]" ^ (make (n - 1)) else "[]"
       in
       let n = degree d in
       v.name ^ (make n)
    | Dep_trm (t, _) -> AstC_to_c.ast_to_string t
    | Dep_ptr _ -> failwith "Dep.to_string: Unsupported dependency type."
  
  (** [Dep.compare d1 d2]: compares two dependencies [d1] and [d2] using their
      string representation, see [Dep.to_string]. *)
  let compare (d1 : t) (d2 : t) : int =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    String.compare d1' d2'
  
  (** [Dep.equal d1 d2]: checks the equality of two dependencies [d1] and
      [d2] based on their string representation. *)
  let equal (d1 : t) (d2 : t) : bool =
    let d1' = to_string d1 in
    let d2' = to_string d2 in
    d1' = d2'
  
  (** [Dep.equal_dimension d1 d2]: checks the equality of two dependencies [d1]
      and [d2] based on their strict representation in the [Dimension] mode (see
      [Dep.to_string]). *)
  let equal_dimension (d1 : t) (d2 : t) : bool =
    let d1' = to_string ~mode:Dimension d1 in
    let d2' = to_string ~mode:Dimension d2 in
    d1' = d2'
  
  (** [Dep.equal_variable d1 d2]: checks the equality of two dependencies [d1]
      and [d2] based on their strict representation in the [Veriable] mode (see
      [Dep.to_string]). *)
  let equal_variable (d1 : t) (d2 : t) : bool =
    let d1' = to_string ~mode:Variable d1 in
    let d2' = to_string ~mode:Variable d2 in
    d1' = d2'
  
  (** [Dep.hash d]: computes the hash of the dependency [d] based on its string
      representation. *)
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
      the appropriate equality check (see [Dep.equal], [Dep.equal_dimension] and
      [Dep.equal_variable]). *)
  let inter2 (s1 : t) (d1sf : (Dep.t -> bool))
        (s2 : t) (d2sf : (Dep.t -> bool)) : t =
    fold (fun d1 res ->
        let d1t = Dep.is_trm d1 in
        let d1s = d1sf d1 in
        let c =
          exists (fun d2 ->
              if d1t && (Dep.is_trm d2) then
                if d1s && (d2sf d2) then
                  Dep.equal d1 d2
                else
                  Dep.equal_dimension d1 d2
              else
                Dep.equal_variable d1 d2) s2
        in
        if c then add d1 res else res) s1 empty
  
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
        (** If a previous binding exists, we compute the union of the sets of
            dependency attributes. *)
        | Some das' ->
           let das'' = DepAttr_set.union das das' in
           add d das'' acc
        (** Otherwise, we create a new binding. *)
        | None ->
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
          (** If a previous binding exists, we compute the union of the sets of
              dependency attributes. *)
          | Some das' ->
             let das'' = DepAttr_set.union das das' in
             add d das'' acc
          (** Otherwise, we create a new binding. *)
          | None ->
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
