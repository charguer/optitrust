open Ast
open Typ
open Apac_dep
open Apac_tasks

(** [FunctionRecord]: a module to represent function records. *)
module FunctionRecord : sig
  type a
  type s = (int * varkind) Var_Hashtbl.t
  type t = {
      mutable args : (a * int * bool) list;
      mutable graph : TaskGraph.t;
      scope : s;
      writes : Var_set.t;
      ast : trm
    }
  val kind : typ -> varkind
  val create : (var * typ) list -> (var * typ) list -> Var_set.t -> trm -> t
  val constify : bool list -> (a * int * bool) list -> (a * int * bool) list
  val is_rw : a -> bool
  val to_string : t -> string
end = struct
    (** [FunctionRecord.a]: an enumeration of function argument access
        classifiers. *)
    type a =
      (** The function {b alters} the argument by side-effect. *)
      | ReadWrite
      (** The function {b does not alter} the argument by side-effect. *)
      | ReadOnly

    (** [FunctionRecord.s]: a type of hash table where the keys are variables
        (see [!type:var]) and the values are pairs where the first element is
        the numbers of levels of indirection of the variables (see [!type:int])
        and the second element is the kind of the variable (see
        [!type:varkind]). For example, in the case of a variable [data] we would
        declare in C as [int ** data] ([int *** data] in the OptiTrust syntax),
        the corresponding entry in a hash table of type [!type:s] would be the
        key-value pair ([\{ name: "data", ... \}], 3, [Var_mutable]). *)
    type s = (int * varkind) Var_Hashtbl.t
    
    (** [FunctionRecord.t]: a type of function records. *)
    type t = {
        (** [args]: a list storing the access classifications (see [!type:a]) of
            arguments and their respective number of levels of indirection. List
            indices match the positions of the arguments in the function
            definition. In the case of class member methods where the 0-th
            argument refers to the current object instance, i.e. [this] in C++,
            the 1-st argument becomes the 0-th argument in the corresponding
            function record. In other terms, the [args] list skips [this]. *)
        mutable args : (a * int * bool) list;
        (** [graph]: the task candidate graph intermediate representation of the
            function (see [!module:TaskGraph]). *)
        mutable graph : TaskGraph.t;
        (** [scope]: a hash table of function-local variables, including the
            arguments, and their number of levels of indirection (see
            [!type:s]). For example, in the case of the following function
            definition,
            
            {[void f(int a, int * tab) {
              float b;
              void ** data;
              ...
            }]}
            
            we would find
            
            a -> 0
            tab -> 1
            b -> 0
            data -> 2
            
            in [scope]. *)
        scope : s;
        (** [writes]: a set of global variables the function writes to. *)
        writes : Var_set.t;
        (** [ast]: a copy of the original abstract syntax tree intermediate
            representation of the function (see [!type:trm]). *)
        ast : trm
      }

    (** [FunctionRecords.classify ty]: attributes an access classification to a
        function argument of type [ty]. A function argument is [ReadOnly] when
        [ty] is:
      
          - a primitive type such as [int],
          - a pointer or a reference to constant types (primitives, references
            or pointers), i.e. the number of inner constant types in [ty] is the
            same as the number of levels of indirection of [ty],
          - an array of constant elements,
          - a constant user-defined type.

        Otherwise, the argument becomes [ReadWrite]. *)
    let classify (ty : typ) : a =
      (** [FunctionRecords.classify.count ty]: recursively count the number [cc]
          of constant types and the number [nli] of levels of indirections
          within the type [ty]. *)
      let rec count (cc : int) (nli : int) (ty : typ) : int * int =
        match ty.typ_desc with
        (** When [ty] is a pointer, we have a new level of indirection. *)
        | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } ->
           count cc (nli + 1) ty
        (** When [ty] is a constant type, increment the [cc] counter. *)
        | Typ_const ty -> count (cc + 1) nli ty
        (** Otherwise, return the counts. *)
        | _ -> (cc, nli)
      in
      match ty.typ_desc with
      (** If [ty] is constant, check if all the inner types are constant too,
          i.e. if the number of inner constant types in [ty] is the same as the
          number of levels of indirection in [ty]. If so, the argument is
          [ReadOnly]. *)
      | Typ_const ty ->
         let (cc, nli) = count 0 0 ty in
         if cc = nli then ReadOnly else ReadWrite
      (** If [ty] is a pointer or a reference, check if all the inner types are
          constant, i.e. if the number of inner constant types in [ty] is the
          same as the number of levels of indirection in [ty]. If so, the
          argument is [ReadOnly]. *)
      | Typ_ptr { ptr_kind = _; inner_typ = ty } ->
         let (cc, nli) = count 0 1 ty in
         if cc = nli then ReadOnly else ReadWrite
      (** If [ty] is an array, check if the inner data type is constant. If so,
          the argument is [ReadOnly]. *)
      | Typ_array _ ->
         if (Typ.is_typ_const (Typ.get_inner_array_type ty)) then
           ReadOnly
         else ReadWrite
      (** If [ty] is a user-defined non-constant type, the argument is not
          [ReadOnly]. *)
      | Typ_constr _ -> ReadWrite
      (** If [ty] is none of the above, it represents a primitive data type. In
          this case, the argument is passed by value and is thus [ReadOnly]. *)
      | _ -> ReadOnly

    (** [FunctionRecord.kind ty]: determines the variable kind (see
        [!type:varkind]) according to the type [ty]. *)
    let kind (ty : typ) : varkind =
      if is_typ_const (get_inner_array_type ty) then Var_immutable
      else Var_mutable
    
    (** [FunctionRecord.create args globs writes ast]: creates a new function
        record based on the arguments in the list [args], the global variables
        in the list [globs], the set [writes] of global variables the function
        writes to within its body and the corresponding abstract syntax tree
        [ast]. *)
    let create (args : (var * typ) list) (globs : (var * typ) list)
          (writes : Var_set.t) (ast : trm) : t =
      (** Create a hash table for function-local variables. *)
      let scope = Var_Hashtbl.create 10 in
      (** For each function argument [arg] in [args], *)
      let args =
        List.mapi (fun i (arg, ty) ->
            (** determine its number of levels of indirection, *)
            let nli = Apac_miscellaneous.typ_get_nli ty in
            (** determine whether its type [ty] is an atomic type or an alias to
                an atomic type, *)
            let atomic =
              let inner = get_inner_const_type ty in
              (is_atomic_typ inner) || (Apac_miscellaneous.typ_is_alias inner)
            in
            (** add it to the hash table of function-local variables, *)
            Var_Hashtbl.add scope arg (nli, Var_immutable);
            (** classify its access (see [!FunctionRecords.classify]) and return
                the classification together with the number of levels of
                indirection and the flag telling whether we can consider this
                argument for execution time profiling (see [!Apac_profiling]).
                This is the case when the argument is a simple variable, i.e.
                its [nli] is less than one, of [atomic] type. *)
            (classify ty, nli, nli < 1 && atomic)) args in
      (** Add the global variables from [globs] to the [scope] of the function
          so as to make them available during dependency discovery (see
          [!Apac_task_candidate_discovery.trm_discover_dependencies]). *)
      List.iter (fun (glob, ty) ->
          (** For this, we have to determine the number of levels of indirection
              of each global variable as well as its kind and *)
          let nli = Apac_miscellaneous.typ_get_nli ty in
          let kind = kind ty in
          (** add it to the hash table of function-local variables. *)
          Var_Hashtbl.add scope glob (nli, kind)
        ) globs;
      {
        args = args;
        graph = Apac_tasks.TaskGraph.create ();
        scope = scope;
        writes = writes;
        ast = ast
      }
    
    (** [FunctionRecord.constify const args]: the function takes as argument a
        list of booleans [const] having the same length as a list of arguments
        [args] following the format of the [args] member of
        [!type:FunctionRecord.t]. Like in the case of [args], the indices of
        [const] match the positions of the arguments of the corresponding
        function. [true] values in [const] mean we can change the access
        classification of the matching argument in [args] from [ReadWrite] to
        [ReadOnly]. If [const] and [args] do not have the same length, fail. *)
    let constify (const : bool list)
          (args : (a * int * bool) list) : (a * int * bool) list =
      if (List.length const) <> (List.length args) then
        failwith "Apac_records.FunctionRecord.constify: incompatible lists of \
                  arguments."
      else List.map2 (fun c (a, nli, p)  ->
               if c then (ReadOnly, nli, p) else (a, nli, p)
             ) const args

    (** [FunctionRecord.is_rw arg i]: checks whether the access classification
        of the argument [arg] is [ReadWrite]. *)
    let is_rw (arg : a) : bool = arg = ReadWrite

    (** [FunctionRecord.to_string record]: returns a string representation of
        the function [record]. *)
    let to_string (record : t) : string =
      let string_of_a_nli (arg : a * int * bool) : string =
        let encode p = if p then ", P" else "" in
        match arg with
        | (ReadWrite, nli, profilable) ->
           "ReadWrite(" ^ string_of_int(nli) ^ (encode profilable) ^ ")"
        | (ReadOnly, nli, profilable) ->
           "ReadOnly(" ^ string_of_int(nli) ^ (encode profilable) ^ ")"
      in
      let string_of_varkind (vk : varkind) : string =
        match vk with
        | Var_mutable -> "mutable"
        | Var_immutable -> "immutable"
      in
      let output = "{\n\targs: [" in
      let n = List.length record.args in
      let args = if n > 0 then
                   (string_of_a_nli (List.hd record.args)) ^
                     (if n > 1 then
                        List.fold_left
                          (fun acc a -> acc ^ "," ^ (string_of_a_nli a))
                          "" (List.tl record.args)
                      else "")
                 else "" in
      let output = output ^ args ^ "],\n\tgraph: " ^
                     (if (Apac_tasks.TaskGraph.is_empty record.graph)
                      then "empty" else "ready") ^ ",\n\twrites: [" in
      let writes = var_set_to_list record.writes in
      let n = List.length writes in
      let writes = if n > 0 then
                     (var_to_string (List.hd writes)) ^
                       (if n > 1 then
                          List.fold_left (fun acc v ->
                              acc ^ "," ^ (var_to_string v)
                            ) "" (List.tl writes)
                        else "")
                   else "" in
      let output = output ^ writes ^ "],\n\tscope: [\n" in
      let n = Var_Hashtbl.length record.scope in
      let scope = if n > 0 then
                    Var_Hashtbl.fold (fun k (nli, kind) acc ->
                        acc ^ "\t\t(" ^
                          (var_to_string k) ^ ", " ^ (string_of_int nli) ^
                            ", " ^ (string_of_varkind kind) ^
                            ")\n") record.scope ""
                  else "" in
      output ^ scope ^ "\t],\n\tast: \"" ^
        (Apac_miscellaneous.excerpt record.ast) ^ "\",\n}"
      
end

(** [restore_cfeatures scope t]: given the [scope], this function translates
    memory accesses within the term [t] from the OptiTrust syntax back to C. *)
let restore_cfeatures (scope : FunctionRecord.s) (t : trm) : trm =
  let open Trm in
  let caddress (t : trm) : trm = Ast_fromto_AstC.caddress_intro_aux false t in
  let rec stackvars (t : trm) : trm =
    trm_simplify_addressof_and_get
      begin match t.desc with
      | Trm_var (vk, v) ->
         let ((_, vk), _) = Var_Hashtbl.find_or_default scope v (0, vk) in
         if vk = Var_mutable then
           trm_address_of (trm_replace (Trm_var (Var_mutable, v)) t)
         else t
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_},
                  [{desc = Trm_var (vk, v); _} as t']) ->
         let ((_, vk), _) = Var_Hashtbl.find_or_default scope v (0, vk) in
         if vk = Var_mutable then t' else trm_map stackvars t
      | _ -> trm_map stackvars t
      end
  in
  stackvars (caddress t)

(** [functions]: a hash table of function records with an initial size of 10
    entries (see [!module:Var_Hashtbl] and [!module:FunctionRecord]). *)
let functions : FunctionRecord.t Var_Hashtbl.t = Var_Hashtbl.create 10

(** [globals]: a map of global variables to their types and flags indicating
    whether they are written to from within a parallel region. *)
let globals : (typ * bool) Var_map.t ref = ref Var_map.empty
