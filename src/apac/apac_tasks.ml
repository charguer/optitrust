(** The automatic parallelization method of APAC involves the creation of a task
    graph representing the input source code. We create one task graph per
    program scope. This way, to a function body containing, for example, a
    for-loop, we will associate one task graph representing the instructions of
    the function's body. A second task graph will be associated to the node of
    the first graph representing the for-loop instruction. This second task
    graph will reflect the instructions of the for-loop body.

    Our task graph implementation relies on the Ocamlgraph library and consists
    of multiple, sometimes mutually dependent, modules defined in this file. *) 
open Ast
open Graph
open Apac_dep

(** [TaskAttr]: a module to represent task candidate attributes. See [Task]. *)
module TaskAttr : sig
  type t =
    | ExitPoint
    | IsJump
    | Singleton
    | Taskifiable
    | WaitForSome
    | WaitForAll
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val to_string : t -> string
end = struct
  type t =
    (** The task candidate represents the [Apac_macros.goto_label] we introduce
        during the 'return' replacement [Apac_basic.use_goto_for_return]. *)
    | ExitPoint
    (** The task candidate involves a jump to the [Apac_macros.goto_label]. *)
    | IsJump
    (** Never merge the task candidate with other task candidates. *)
    | Singleton
    (** Mark the task candidate as eligible to become a parallelizable task. *)
    | Taskifiable
    (** Instead of becoming a parallelizable task, the task candidate should
        translate into a synchronization barrier with explicit data
        dependencies. *)
    | WaitForSome
    (** Instead of becoming a parallelizable task, the task candidate should
        translate into a global synchronization barrier. *)
    | WaitForAll
  (** [TaskAttr.compare ta1 ta2]: compares two task attributes [ta1] and [ta2].
      As [TaskAttr.t] is an enumeration type, this is a simple comparison
      between two integer values representing enumeration labels. *)
  let compare (ta1 : t) (ta2 : t) : int =
    if ta1 = ta2 then 0
    else if ta1 > ta2 then 1
    else -1
  (** [TaskAttr.equal ta1 ta2]: checks the equality of two task attributes [ta1]
      and [ta2]. See [TaskAttr.compare]. *)
  let equal (ta1 : t) (ta2 : t) : bool = compare ta1 ta2 = 0
  (** [TaskAttr.to_string ta]: returns a string representation of the task
      attribute [ta]. *)
  let to_string (ta : t) : string =
    match ta with
    | ExitPoint -> "ExitPoint"
    | IsJump -> "IsJump"
    | Singleton -> "Singleton"
    | Taskifiable -> "Taskifiable"
    | WaitForSome -> "WaitForSome"
    | WaitForAll -> "WaitForAll"
end

(** [TaskAttr_set]: a module to represent sets of task candidate attributes. *)
module TaskAttr_set = struct
  include Set.Make(TaskAttr)
  (** [TaskAttr_set.union2 tas1 tas2]: same as [TaskAttr_set.union], but if the
      union of [tas1] and [tas2] contains both the [WaitForSome] and the
      [WaitForAll] attributes, we keep only the latter. *)
  let union2 (tas1 : t) (tas2 : t) : t =
    let tas = union tas1 tas2 in
    if mem WaitForAll tas then remove WaitForSome tas else tas
  (** [TaskAttr_set.to_string tas]: returns a string representation of the set
      of task attributes [tas]. *)
  let to_string (tas : t) : string =
    fold (fun ta acc -> acc ^ (TaskAttr.to_string ta) ^ " ") tas " "
end

(** [Task]: a module to represent a node of a task graph. The initial task graph
    of a function considers each instruction and each block of instructions
    (loops, scopes, switches, ...) as a separate task. While building the
    initial task graph of a function, we have to compute the potential data
    dependencies between the future tasks. During this process, we iterate over
    the local AST of the target function, look for data dependencies, construct
    the output graph nodes and add edges between the nodes based on the
    discovered dependencies. Each [Task] node may represent a task by itself.
    Nodes can be merged or removed. *)
module rec Task : sig
         type t = {
             mutable schedule : int;
             mutable current : trms;
             mutable attrs : TaskAttr_set.t;
             mutable scope : int Var_map.t;
             mutable ins : Dep_set.t;
             mutable inouts : Dep_set.t;
             mutable ioattrs : ioattrs_map;
             mutable children : TaskGraph.t list list;
             mutable cost : trm option
           }
         val create :
           int -> trm -> TaskAttr_set.t -> int Var_map.t -> Dep_set.t ->
           Dep_set.t -> ioattrs_map -> TaskGraph.t list list -> t
         val copy : t -> t
         val depending : t -> t -> bool
         val attributed : t -> TaskAttr.t -> bool
         val subscripted : t -> bool
         val merge : t -> t -> t
         val empty : unit -> t
         val to_excerpt : t -> string
         val to_string : t -> string
         val to_label : t -> string
       end = struct
  (** A task graph node is represented by: *)
  type t = {
      (** - a logical schedule allowing us to preserve the original
          lexicographic order of statements in the output program (-1 means that
          the task candidate has no valid schedule), *)
      mutable schedule : int;
      (** - a list of AST terms associated with it (initially, only one term is
          in the list; multiple terms may appear after merging of two or more
          nodes), *)
      mutable current : trms;
      (** - a set of attributes which help us to translate the task into
          taskified source code (see [TaskAttr] as well as the `backend.ml'
          file), *)
      mutable attrs : TaskAttr_set.t;
      (** - a map of variables, to their respective numbers of levels of
          indirections, present in the scope of the task candidate, *)
      mutable scope : int Var_map.t;
      (** - a set of input (read only) data dependencies (see [Dep]), *)
      mutable ins : Dep_set.t;
      (** - a set of input-output (read-write) data dependencies (see [Dep]), *)
      mutable inouts : Dep_set.t;
      (** - a map of data dependencies to sets of dependency attributes (see
          [ioattrs_map]), *)
      mutable ioattrs : ioattrs_map;
      (** - a list of lists of nested task graphs (each term in [current] may
          have more than one child task graph associated to it, i.e. a then and
          an else branch in the case of an if-conditional statement). *)
      mutable children : TaskGraph.t list list;
      (** - an optional cost model, i.e. a polynomial expression in the form of
          an abstract syntax tree term, allowing us to estimate the execution
          time of the task candidate at run-time. *)
      mutable cost : trm option
    }
  
  (** [Task.create schedule current attrs scope ins inouts children]: creates a
      new task candiate based on the logical [schedule], the [current] AST term,
      the set of task [attrs], the current [scope] of variables, the set of
      input dependencies [ins], the set of input-output dependencies [inouts]
      and the list of lists of nested task graphs. *)
  let create (schedule : int) (current : trm) (attrs : TaskAttr_set.t)
        (scope : int Var_map.t) (ins : Dep_set.t) (inouts : Dep_set.t)
        (ioattrs : ioattrs_map) (children : TaskGraph.t list list) : t =
    (** Filter out the input dependencies on variables that are not defined in
        the current scope. *)
    let ins' = Dep_set.filter (
                   fun d -> match d with
                            | Dep_var v -> Var_map.mem v scope
                            | Dep_trm (_, v) -> Var_map.mem v scope
                            | _ -> false
                 ) ins in
    (** Filter out the input-output dependencies on variables that are not
        defined in the current scope. *)
    let inouts' = Dep_set.filter (
                      fun d -> match d with
                               | Dep_var v -> Var_map.mem v scope
                               | Dep_trm (_, v) -> Var_map.mem v scope
                               | _ -> false
                    ) inouts in
    (** Filter out the attributes for dependencies on variables that are not
        defined in the current scope. *)
    let ioattrs' = Dep_map.filter (
                      fun d _ -> match d with
                               | Dep_var v -> Var_map.mem v scope
                               | Dep_trm (_, v) -> Var_map.mem v scope
                               | _ -> false
                    ) ioattrs in
    (** The dependency analysis may conclude that a dependency is both an input
        and an input-output dependency. To simplify further computations on
        dependencies, we remove this kind of dependencies from the set of input
        dependencies and keep them only in the set of input-output
        dependencies. *)
    let ins' = Dep_set.diff ins' inouts' in
    {
      schedule = schedule;
      (** Initially, a task is represented by a single instruction. *)
      current = [current];
      attrs = attrs;
      scope = scope;
      ins = ins';
      inouts = inouts';
      ioattrs = ioattrs';
      children = children;
      cost = None
    }

  (** [Task.copy task]: creates a copy of the [task]. *)
  let copy (task : t) = {
      schedule = task.schedule;
      current = task.current;
      attrs = task.attrs;
      scope = task.scope;
      ins = task.ins;
      inouts = task.inouts;
      ioattrs = task.ioattrs;
      children = task.children;
      cost = task.cost
    }

  (** [Task.depending t1 t2]: checks whether the task [t2] depends on the task
      [t1]. *)
  let depending (t1 : t) (t2 : t) : bool =
    let dsf (ioa : ioattrs_map) (d : Dep.t) : bool =
      Dep_map.has_with_attribute d Subscripted ioa
    in
    let i1 =
      Dep_set.inter2 t1.inouts (dsf t1.ioattrs) true
        t2.ins (dsf t2.ioattrs) false in
    let i2 =
      Dep_set.inter2 t1.ins (dsf t1.ioattrs) false
        t2.inouts (dsf t2.ioattrs) true in
    let i3 =
      Dep_set.inter2 t1.inouts (dsf t1.ioattrs) true
        t2.inouts (dsf t2.ioattrs) true in
    not ((Dep_set.is_empty i1) && (Dep_set.is_empty i2) &&
           (Dep_set.is_empty i3))

  (** [Task.attributed task attr]: checks whether the task [task] carries the
      attribute [attr]. *)
  let attributed (task : t) (attr : TaskAttr.t) : bool =
    TaskAttr_set.mem attr task.attrs
  
  (** [Task.subscripted task]: checks whether at least one of the dependencies
      of the task [task] has the [Subscripted] attribute (see [TaskAttr.t]). *)
  let subscripted (task : t) : bool =
    Dep_map.exists (fun _ das -> DepAttr_set.mem Subscripted das) task.ioattrs
  
  (** [Task.merge t1 t2]: merges two tasks into a new single task. *)
  let merge (t1 : t) (t2 : t) : t =
    (** [Task.merge.xor a b]: computes the exclusive OR of [a] and [b]. *)
    let xor (a : bool) (b : bool) : bool = (a || b) && not (a && b) in
    (** For this, we concatenate the lists of associated AST terms, *)
    let current' = t1.current @ t2.current in
    (** compute the union of attributes, *)
    let attrs' = TaskAttr_set.union t1.attrs t2.attrs in
    (** compute the union of the scopes, *)
    let scope' = Var_map.union (fun k v1 v2 -> Some v1) t1.scope t2.scope in
    (** compute the union of the input dependency sets, *)
    let ins' = Dep_set.union t1.ins t2.ins in
    (** compute the union of the input-output dependency sets, *)
    let inouts' = Dep_set.union t1.inouts t2.inouts in
    (** remove from the set of input dependencies the dependencies that are both
        input and input-output dependencies, *)
    let ins' = Dep_set.diff ins' inouts' in
    (** compute the union of maps of dependency attributes, *)
    let ioattrs' = Dep_map.union2 t1.ioattrs t2.ioattrs in
    (** concatenate the list of lists of nested graphs of [t1] and [t2], *)
    let children' = t1.children @ t2.children in
    (** determine the cost model of the resulting task candidate, if any. *)
    let cost' = match (t1.cost, t2.cost) with
      (** When both [t1] and [t2] have a cost model, i.e. respectively [c1] and
          [c2], the cost model of the resulting task candidate is the sum of
          [c1] and [c2]. *)
      | (Some c1, Some c2) -> Some (Trm.trm_add c1 c2)
      (** When either only [t1] or only [t2] has a cost model, the latter
          becomes the cost model of the resulting task candidate. *)
      | (Some c, None)
        | (None, Some c) -> Some c
      (** When nor [t1] nor [t2] has a cost model, the resulting task candidate
          won't have a cost model either. *)
      | (None, None) -> None in
    (** If we are merging a suitable task candidate with an unsuitble task
        candidate, the result is a suitable task candidate. *)
    let attrs' =
      if xor (attributed t1 WaitForSome) (attributed t2 WaitForSome) then
        TaskAttr_set.remove WaitForSome attrs' else attrs' in
    {
      schedule = t1.schedule;
      current = current';
      attrs = attrs';
      scope = scope';
      ins = ins';
      inouts = inouts';
      ioattrs = ioattrs';
      children = children';
      cost = cost'
    }
  
  (** [Task.empty]: produces an empty task having no AST term associated with it
      as well as without any attributes, dependencies or nested graphs. *)
  let empty () = {
      schedule = -1;
      current = [];
      attrs = TaskAttr_set.empty;
      scope = Var_map.empty;
      ins = Dep_set.empty;
      inouts = Dep_set.empty;
      ioattrs = Dep_map.empty;
      children = [];
      cost = None
    }

  (** [Task.to_excerpt task]: returns an excerpt of a string representation of
      the first statement in [task]. *)
  let to_excerpt (task : t) : string =
    Apac_miscellaneous.excerpt (List.hd task.current)
  
  (** [Task.to_string task]: returns a string representation of [task]. *)
  let to_string (task : t) : string =
    let what = (string_of_int task.schedule) ^ ": " ^
                 List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ " > " ^ desc
                 ) "" task.current in
    let ins = Dep_set.fold
                (fun a acc ->
                  let ioa = Dep_map.find_opt a task.ioattrs in
                  let ioa = match ioa with
                    | Some das -> "(" ^ (DepAttr_set.to_string das) ^ ")"
                    | None -> ""
                  in
                  let ioa = if ioa <> "" then " " ^ ioa else ioa in
                  acc ^ " " ^ (Dep.to_string a) ^ ioa) task.ins "" in
    let inouts = Dep_set.fold (
                     fun a acc -> 
                     let ioa = Dep_map.find_opt a task.ioattrs in
                     let ioa = match ioa with
                       | Some das -> "(" ^ (DepAttr_set.to_string das) ^ ")"
                       | None -> ""
                     in
                     let ioa = if ioa <> "" then " " ^ ioa else ioa in
                     acc ^ " " ^ (Dep.to_string a) ^ ioa
                   ) task.inouts "" in
    let excerpt = to_excerpt task in
    what ^ "[ " ^ excerpt  ^ " ]" ^
      " (in: [" ^ ins ^ " ], inout: [" ^ inouts ^ " ]) (" ^
        (TaskAttr_set.to_string task.attrs) ^ ")\n"
  
  (** [Task.to_label task]: returns a string representation of [task] used when
      converting a task graph into the Dot text format. See
      [TaskGraphPrinter]. *)
  let to_label (task : t) : string =
    let what = (string_of_int task.schedule) ^ ": " ^
                 List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ " > " ^ desc
                 ) "" task.current in
    let excerpt = to_excerpt task in
    what ^ "\\n" ^ (TaskAttr_set.to_string task.attrs) ^ "\\n" ^ excerpt
end and TaskGraph : Sig.IM
        with type V.label = Task.t = Imperative.Digraph.Abstract(Task)

(** [TaskGraphPrinter]: a module to convert a [TaskGraph] into a text
    representation such as the Dot format. See [DotExport]. *)
module TaskGraphPrinter = struct
  (** This module actually extends the original [TaskGraph] module. *)
  include TaskGraph
  (** For [TaskGraphPrinter] to work with the export modules, e.g. [DotExport],
      it must implement the following methods. *)
  (** [TaskGraphPrinter.vertex_name vertex]: returns a string representation of
      [vertex] in a [TaskGraph]. *)
  let vertex_name (vertex : V.t) : string =
    (** Get the [Task] corresponding to [vertex]. *)
    let task = TaskGraph.V.label vertex in
    (** Determine the kinds of the AST terms associated with the task and *)
    let what = List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ "_" ^ desc
                 ) "" task.current in
    (** append a unique identifier, i.e. the hash of [vertex], to it. *)
    what ^ "_" ^ (string_of_int (TaskGraph.V.hash vertex))
  (** [TaskGraphPrinter.vertex_attributes vertex]: associates a list of
      attributes to a given [vertex] of a [TaskGraph]. Here, we consider only
      the [Label] attributed, which is produced using the [to_label] method of
      [Task]. *)
  let vertex_attributes
        (vertex : V.t) : Graphviz.DotAttributes.vertex list =
    [ `Label (Task.to_label (TaskGraph.V.label vertex) ) ]
  (** [TaskGraphPrinter.get_nested_graphs vertex]: gather the nested graphs of
      [vertex]. *)
  let get_nested_graphs (vertex : V.t) =
    let task = TaskGraph.V.label vertex in
    List.concat task.children
  (** We leave the following methods without a useful implementation as we do
      not need to use them. However, they still have to be 'implemented'. *)
  let get_subgraph (vertex : V.t) = None
  let graph_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.graph list = []
  let default_vertex_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.vertex list = []
  let default_edge_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.edge list = []
  let edge_attributes
        (edge : E.t) : Graphviz.DotAttributes.edge list = []
  (** However, we implement custom printing methods that are not required by
      [DotExport] but are useful for debugging purposes. *)
  (** [TaskGraphPrinter.to_string g]: returns a string representation of a
      possibly nested task graph [g]. *)
  let to_string (g : TaskGraph.t) : string =
    let rec aux (g : TaskGraph.t) (indent : string) : string =
      TaskGraph.fold_vertex (fun v acc ->
          let vl : Task.t = TaskGraph.V.label v in
          let sg : string = List.fold_left (fun acc gl ->
                                let og = List.fold_left (fun acc g' ->
                                             let o = aux g' (indent ^ "    ") in
                                             acc ^ o) "" gl
                                in acc ^ og) "" vl.children in
          acc ^ indent ^ (string_of_int (TaskGraph.V.hash v)) ^ ": " ^
            (Task.to_string vl) ^ sg) g ""
    in
    aux g ""
  (** [TaskGraphPrinter.print g]: prints a string representation of a possibly
      nested task graph [g] onto the standard output. *)
  let print (g : TaskGraph.t) : unit =
    let gs = to_string g in
    Printf.printf "%s\n" gs
end

(** [TaskGraphExport]: a module for exporting a [TaskGraph] into the Dot and the
    Pdf formats through the [TaskGraphPrinter] module. *)
module TaskGraphExport = struct
  include Graph.Graphviz.Dot(TaskGraphPrinter)
  (** [to_dot g f]: exports the task candidate graph [g] into a Dot file [f]. *)
  let to_dot (g : TaskGraph.t) (f : string) : unit =
    let f = open_out f in output_nested_graph f g; close_out f

  (** [to_pdf g f]: exports the task candidate graph [g] into a Pdf file [f]. *)
  let to_pdf (g : TaskGraph.t) (f : string) : unit =
    (** Get the base name part [n] of [f]. *)
    let n = Filename.basename f in
    (** Export [g] to a temporary Dot file [tmp]. *)
    let tmp = Filename.temp_file (Filename.remove_extension n) "dot" in
    to_dot g tmp;
    (** Export [tmp] to [f] using Graphviz. *)
    ignore (Sys.command ("dot -Tpdf -o " ^ f ^ " " ^ tmp))
end

(** [TaskGraphBuilder]: a module allowing us to make the [TaskGraphOper]
    module. *)
module TaskGraphBuilder = Builder.I(TaskGraph)

(** [TaskGraphOper]: a module providing various graph operations for
    [TaskGraph]s. *)
module TaskGraphOper = struct
  include Oper.Make(TaskGraphBuilder)
  let rec recursive_transitive_reduction (g : TaskGraph.t) : TaskGraph.t =
    let g' = transitive_reduction g in
    TaskGraph.iter_vertex (fun v ->
        let vl : Task.t = TaskGraph.V.label v in
        vl.children <- List.map (fun gs ->
                     List.map (fun g' -> recursive_transitive_reduction g') gs
                         ) vl.children) g';
    g'
  let rec propagate_dependency_attribute (das : DepAttr_set.t) (ds : Dep_set.t)
            (g : TaskGraph.t) : unit =
    TaskGraph.iter_vertex (fun v ->
        let v' : Task.t = TaskGraph.V.label v in
        v'.ioattrs <- Dep_map.may_bind_set ds das v'.ioattrs
                        (fun d ->
                          (Dep_set.mem d v'.ins) ||
                            (Dep_set.mem d v'.inouts));
        List.iter (fun gs ->
            List.iter (fun g' ->
                (propagate_dependency_attribute das ds) g'
              ) gs
          ) v'.children) g
  (** [TaskGraphOper.root g]: find the root node of the task graph [g], i.e. the
      node without any predecessor. *)
  let root (g : TaskGraph.t) : TaskGraph.V.t =
    (** Traverse the graph [g] and find the node without predecessors. This will
        be the root node. *)
    let vs = TaskGraph.fold_vertex (fun v acc ->
                 if TaskGraph.in_degree g v < 1 then v::acc else acc
               ) g [] in
    List.hd vs
end

(** [TaskGraphTraverse]: a module implementing a traversal function for task
    graphs. *)
module TaskGraphTraverse : sig
  type t = Strict | Priority | Relaxed
  val fold : TaskGraph.t -> TaskGraph.V.t list
  val to_ast : (TaskGraph.V.t -> trms) -> TaskGraph.t -> trms
  val iter : (TaskGraph.V.t -> unit) -> TaskGraph.t -> unit
  val iter_schedule : (TaskGraph.V.t -> unit) -> TaskGraph.t -> unit
end = struct
  (** [TaskGraphTraverse.H]: a hash table module for task graph vertices. *)
  module H = Hashtbl.Make(TaskGraph.V)
  
  (** [TaskGraphTraverse.t]: enumeration of algorithmic variants for traversing
      task candidate graphs. *)
  type t =
    (** Follow the original lexical order of statements in the input program. *)
    | Strict
    (** Perform a breadth-first traversal, but when a task candidate [v] belongs
        to a sequence of inter-dependent vertices, where the first vertex has
        exactly one successor, the last vertex has exactly one predecessor and
        all the intermediate vertices have exactly one predecessor and exactly
        one successor, visit the entire sequence in addition to [v]. *)
    | Priority
    (** Perform a conventional breadth-first traversal. *)
    | Relaxed
  
  (** [TaskGraphTraverse.seq v g]: starting from the vertex [v] in the task
      candidate graph [g], find the longest sequence of inter-dependent
      vertices, where the first vertex has exactly one successor, the last
      vertex has exactly one predecessor and all the intermediate vertices have
      exactly one predecessor and exactly one successor. The resulting sequence
      always contains at least one element, which is [v]. Note that this is a
      module-local function, i.e. the signature of [TaskGraphTraverse] does not
      expose it. See [TaskGraphTraverse.codify] and [TaskGraphTraverse.iter] for
      the usage context. *)
  let rec seq (v : TaskGraph.V.t) (g : TaskGraph.t) : TaskGraph.V.t list =
    (** Retrieve the list of successor of [v] in [g]. *)
    let n = TaskGraph.succ g v in
    (** If [v] has more than one successor, we can not build a sequence with
        more than one element, i.e. the vertex [v]. *)
    if (List.length n) <> 1 then [v]
    else
      (** Otherwise, we extract the successor of [v] from the list. *)
      let n = List.hd n in
      (** If the successor has exactly one predecessor, we can continue building
          the resulting sequence. Otherwise, we simply return a sequence
          consisting of [v]. *)
      if (TaskGraph.in_degree g n) < 2 then v :: (seq n g) else [v]
  
  (** [TaskGraphTraverse.fold g]: folds the task candidate graph [g] into a
      single list of task candidate vertices in the ascending order of task
      candidate schedules. *)
  let fold (g : TaskGraph.t) : TaskGraph.V.t list =
    (** Retrieve all the vertices of [g] in a list. *)
    let vs = TaskGraph.fold_vertex (fun v acc -> v::acc) g [] in
    (** Sort the list of vertices in the ascending order following their
        schedules (see [Task.t]) and return the resutling list. *)
    List.sort (fun v1 v2 ->
        let t1 = TaskGraph.V.label v1 in
        let t2 = TaskGraph.V.label v2 in
        Int.compare t1.schedule t2.schedule
      ) vs
  
  (** [TaskGraphTraverse.to_ast f g]: recursively traverses and translates the
      task candidate graph [g] into an abstract syntax tree by applying the
      translation function [f] on each task candidate vertex of [g]. *)
  let rec to_ast (f : (TaskGraph.V.t -> trms)) (g : TaskGraph.t) : trms =
    (** Retrieve all the vertices of [g] in a list following the ascending order
        of their schedules (see [Task.t]). *)
    let vs = fold g in
    (** Discard the virtual root vertex, i.e. the vertex of schedule 0. *)
    let vs = List.tl vs in
    (** Translate all the vertices and return the result. *)
    let open Trm in
    let open Mark in
    List.fold_left (fun acc v ->
        (** Retrieve the label [l] of the current task candidate vertex [v]. *)
        let l = TaskGraph.V.label v in
        (** When [v] involves one or more nested task candidate graphs, we have
            to translate them recursively. *)
        l.current <-
          (** To this end, for an [i]-th statement [s] in [v] (see the [current]
              memeber in [!module:Task.t]), whether it is an iteration or a
              selection statment, i.e. a statement involving substatements, and
              if so, we recursively translate the latter. *)
          List.mapi (fun i s ->
              match s.desc with
              (** [s] is a for-loop, *)
              | Trm_for_c (init, cond, step, _, _) ->
                 let cg = List.nth l.children i in
                 let cg = List.hd cg in
                 (** translate the task candidate graph [cg] of its [body], *)
                 let body = to_ast f cg in
                 let body = trm_seq (Mlist.of_list body) in
                 (** add the heapification helper mark
                     [!Apac_macros.heapify_breakable_mark] to the [body]'s
                     abstract syntax tree and *)
                 let body = trm_add_mark
                              Apac_macros.heapify_breakable_mark body in
                 (** rebuild the loop term. *)
                 trm_for_c ~annot:s.annot ~ctx:s.ctx init cond step body
              (** [s] is a for-loop, *)
              | Trm_for (range, _, _) ->
                 let cg = List.nth l.children i in
                 let cg = List.hd cg in
                 (** translate the task candidate graph [cg] of its [body], *)
                 let body = to_ast f cg in
                 let body = trm_seq (Mlist.of_list body) in
                 (** add the heapification helper mark
                     [!Apac_macros.heapify_breakable_mark] mark to the [body]'s
                     abstract syntax tree and *)
                 let body = trm_add_mark
                              Apac_macros.heapify_breakable_mark body in
                 (** rebuild the for-term. *)
                 trm_for ~annot:s.annot ~ctx:s.ctx range body
              (** [s] is an if-conditional, *)
              | Trm_if (cond, _, no) ->
                 let cg = List.nth l.children i in
                 let yes = List.nth cg 0 in
                 (** translate the task candidate graph [cg] of its then-branch
                     [yes], *)
                 let yes = to_ast f yes in
                 let yes = trm_seq (Mlist.of_list yes) in
                 (** add the heapification helper mark
                     [!Apac_macros.heapify_mark] to the [yes]'s abstract syntax
                     tree and *)
                 let yes = trm_add_mark Apac_macros.heapify_mark yes in
                 (** if an else-branch [no] is present, process it too. *)
                 let no = if (is_trm_unit no) then
                            trm_unit ()
                          else
                            let no = List.nth cg 1 in
                            let no = to_ast f no in
                            let no = trm_seq (Mlist.of_list no) in 
                            trm_add_mark Apac_macros.heapify_mark no in
                 (** Finally, rebuild the if-term. *)
                 trm_if ~annot:s.annot ~ctx:s.ctx cond yes no
              (** [s] is a while-loop, *)
              | Trm_while (cond, _) ->
                 let cg = List.nth l.children i in
                 let cg = List.hd cg in
                 (** translate the task candidate graph [cg] of its [body], *)
                 let body = to_ast f cg in
                 let body = trm_seq (Mlist.of_list body) in
                 (** add the heapification helper mark
                     [!Apac_macros.heapify_breakable_mark] to the [body]'s
                     abstract syntax tree and *)
                 let body = trm_add_mark
                              Apac_macros.heapify_breakable_mark body in
                 (** rebuild the while-term. *)
                 trm_while ~annot:s.annot ~ctx:s.ctx cond body
              (** [s] is a do-while-loop, *)
              | Trm_do_while (_, cond) ->
                 let cg = List.nth l.children i in
                 let cg = List.hd cg in
                 (** translate the task candidate graph [cg] of its [body], *)
                 let body = to_ast f cg in
                 let body = trm_seq (Mlist.of_list body) in
                 (** add the heapification helper mark
                     [!Apac_macros.heapify_breakable_mark] to the [body]'s
                     abstract syntax tree and *)
                 let body = trm_add_mark
                              Apac_macros.heapify_breakable_mark body in
                 (** rebuild the while-term. *)
                 trm_do_while ~annot:s.annot ~ctx:s.ctx body cond
              (** [s] is a switch-statement, *)
              | Trm_switch (cond, cases) ->
                 let cg = List.nth l.children i in                 
                 (** translate the task candidate graph [bg] of each of its
                     [cases] and add the heapification helper mark
                     [!Apac_macros.heapify_breakable_mark] to its abstract
                     syntax tree. *)
                 let cases =
                   List.map2 (fun (labels, _) bg ->
                       let b = to_ast f bg in
                       let b = trm_seq (Mlist.of_list b) in
                       let b = trm_add_mark
                                 Apac_macros.heapify_breakable_mark b in
                       (labels, b)
                     ) cases cg in
                 (** Finally, rebuild the switch-term. *)
                 trm_switch ~annot:s.annot ~ctx:s.ctx cond cases
              (** When [s] is a compound statements, we just need to mark it
                  with the heapification helper mark
                  [!Apac_macros.heapify_mark]. *)
              | Trm_seq _ -> trm_add_mark Apac_macros.heapify_mark s
              (** In any other case, we leave [s] as is. *)
              | _ -> s
            ) l.current;
        (** Translate the current vertex [v] itself. *)
        acc @ (f v)) [] vs
  
  (** [TaskGraphTraverse.iter_schedule f g]: iterates over the vertices of the
      task graph [g] in the ascending order of their schedules while applying
      the user-provided function [f] on each of them. *)
  let iter_schedule (f : (TaskGraph.V.t -> unit)) (g : TaskGraph.t) : unit =
    (** Retrieve all the vertices of [g] in a list following the ascending order
        of their schedules (see [Task.t]). *)
    let vs = fold g in
    (** Discard the virtual root vertex, i.e. the vertex of schedule 0. *)
    let vs = List.tl vs in
    (** Apply [f] on all the vertices. *)
    List.iter (fun v -> f v) vs
  
  (** [TaskGraphTraverse.iter f g]: iterates over the vertices of the task graph
      [g] while applying the user-provided function [f] on each of them. *)
  let iter (f : (TaskGraph.V.t -> unit)) (g : TaskGraph.t) : unit =
    (** Find and extract the root node. *)
    let r = TaskGraphOper.root g in
    (** Create an hash table for already visited nodes. *)
    let v = H.create 97 in
    (** Create a queue for elements to visit. *)
    let q = Queue.create () in
    let rec visit (root : bool) : unit =
      (** If the queue of elements to visit is empty, we are done. Return the
          list of codified elements. *)
      if not (Queue.is_empty q) then
        begin
          (** Get the first element from the queue of vertices to visit. This
              will be the current element to process. *)
          let hd = Queue.take q in
          (** If all of its predecessors has been visited, i.e. if all of them
              are in the hash table of visited nodes, *)
          let all = TaskGraph.fold_pred (fun p a -> a && H.mem v p) g hd true in
          if all && not (H.mem v hd) then
            begin
              (** visit the current element, *)
              H.add v hd ();
              (** push its successors to the queue of vertices to visit, *)
              TaskGraph.iter_succ (fun e ->
                  (** If a successor is a part of a sequence of nodes we could
                      merge in the future due to in-between data dependencies,
                      push them to the queue of vertices to visit as well. This
                      allows us to further preserve the original lexicographic
                      order of the input source code in the output source
                      code. See [TaskGraphTraverse.seq]. *)
                  let es = seq e g in List.iter (fun f -> Queue.push f q) es
                ) g hd;
              (** apply [f] on the current element, *)
              if (not root) then f hd; 
              (** append it to the list of already codified elements and
                  recurse. *)
              visit false
            end
          (** Otherwise, do nothing and recurse. *)
          else visit false
        end
    in
    (** Push the root element to the queue and *)
    Queue.push r q;
    (** Start the codification process with an empty list of codified
        elements. *)
    visit true
end

