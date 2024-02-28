(* The automatic parallelization method of APAC involves the creation of a task
   graph representing the input source code. We create one task graph per
   program scope. This way, to a function body containing, for example, a
   for-loop, we will associate one task graph representing the instructions of
   the function's body. A second task graph will be associated to the node of
   the first graph representing the for-loop instruction. This second task graph
   will reflect the instructions of the for-loop body.

   Our task graph implementation relies on the Ocamlgraph library and consists
   of multiple, sometimes mutually dependent, modules defined in this file. *) 
open Ast
open Graph
open Apac_dep

(* [TaskAttr]: a module to represent task attributes. See [Task]. *)
module TaskAttr : sig
  type t =
    Singleton | WaitForNone | WaitForSome | WaitForAll | HasJump | ExitPoint
  val compare : t -> t -> int
  val equal : t -> t -> bool
end = struct
  type t =
    (* Never merge the task with other tasks. *)
    | Singleton
    (* Do not create a task. *)
    | WaitForNone
    (* Do not create a task. In the resulting code, prepend the associated
       statements with a synchronisation barrier on selected dependencies. *)
    | WaitForSome
    (* Do not create a task. In the resulting code, prepend the associated
       statements with a global synchronisation barrier. *)
    | WaitForAll
    (* The task contains an unconditional jump, i.e. a [goto]. *)
    | HasJump
    (* The task represents the last instruction before exiting the execution
       sequence, i.e. the [Apac_core.goto_label]. *)
    | ExitPoint
  (* [TaskAttr.compare ta1 ta2]: compares two task attributes [ta1] and [ta2].
     As [TaskAttr.t] is an enumeration type, this is a simple comparison between
     two integer values representing the associated enumeration labels being
     compared. *)
  let compare (ta1 : t) (ta2 : t) : int =
    if ta1 = ta2 then 0
    else if ta1 > ta2 then 1
    else -1
  (* [TaskAttr.equal ta1 ta2]: checks the equality of two task attributes [ta1]
     and [ta2]. See [TaskAttr.compare]. *)
  let equal (ta1 : t) (ta2 : t) : bool = compare ta1 ta2 = 0
end

(* [TaskAttr_set]: a module to represent sets of task attributes. *)
module TaskAttr_set = Set.Make(TaskAttr)

(* [TaskWeight]: a module to represent weights of edges in a task graph. See
   [TaskGraph]. *)
module TaskWeight : sig
  type t = int
  val default : t
  val compare : t -> t -> int
end = struct
  (* A weight of an edge is a simple integer value. *)
  type t = int
  (* The default weight is one. *)
  let default = 1
  (* [TaskWeight.compare tw1 tw2]: compares two edge weights [tw1] and [tw2]. *)
  let compare tw1 tw2 = tw1 - tw2
end

(* [Task]: a module to represent a node of a task graph. The initial task graph
   of a function considers each instruction and each block of instructions
   (loops, scopes, switches, ...) as a separate task. While building the initial
   task graph of a function, we have to compute the potential data dependencies
   between the future tasks. During this process, we iterate over the local AST
   of the target function, look for data dependencies, construct the output
   graph nodes and add edges between the nodes based on the discovered
   dependencies. Each [Task] node may represent a task by itself. Nodes can be
   merged or removed. *)
module rec Task : sig
         type t = {
             current : trms;
             attrs : TaskAttr_set.t;
             mutable ins : Dep_set.t;
             mutable inouts : Dep_set.t;
             children : TaskGraph.t list list;
           }
         val create :
           trm -> TaskAttr_set.t -> Var_set.t -> Dep_set.t -> Dep_set.t ->
           TaskGraph.t list list -> t
         val has_attr : t -> TaskAttr.t -> bool
         val merge : t -> t -> t
         val update : t -> trms -> t
         val empty : unit -> t
         val to_string : t -> string
         val to_label : t -> string
       end = struct
  (* A task graph node is represented by: *)
  type t = {
      (* - a list of AST terms associated with it (initially, only one term is
         in the list; multiple terms may appear after merging of two or more
         nodes), *)
      current : trms;
      (* - a set of attributes which help us to translate the task into
         taskified source code (see [TaskAttr] as well as the `backend.ml'
         file), *)
      attrs : TaskAttr_set.t;
      (* - a set of input (read only) data dependencies (see [Dep]), *)
      mutable ins : Dep_set.t;
      (* - a set of input-output (read-write) data dependencies (see [Dep]), *)
      mutable inouts : Dep_set.t;
      (* - a list of lists of nested task graphs (each term in [current] may
         have more than one child task graph associated to it, i.e. a then and
         an else branch in the case of an if-conditional statement). *)
      children : TaskGraph.t list list;
    }
  (* [Task.create current attrs scope ins inouts children]: creates a new task
     graph node based on the [current] AST term, the set of task [attrs], the
     set of the current scope's variables, the set of input dependencies [ins],
     the set of input-output dependencies [inouts] and the list of lists of
     nested task graphs. *)
  let create (current : trm) (attrs : TaskAttr_set.t) (scope : Var_set.t)
        (ins : Dep_set.t) (inouts : Dep_set.t)
        (children : TaskGraph.t list list) : t =
    (* Filter out the input dependencies on variables that are not defined in
       the current scope. *)
    let ins' = Dep_set.filter (
                   fun d -> match (Dep.to_atomic d) with
                            | Dep_var v -> Var_set.mem v scope
                            | Dep_trm (_, v) -> Var_set.mem v scope
                            | _ -> false
                 ) ins in
    (* Filter out the input-output dependencies on variables that are not
       defined in the current scope. *)
    let inouts' = Dep_set.filter (
                      fun d -> match (Dep.to_atomic d) with
                               | Dep_var v -> Var_set.mem v scope
                               | Dep_trm (_, v) -> Var_set.mem v scope
                               | _ -> false
                    ) inouts in
    (* The dependency analysis may conclude that a dependency is both an input
       and an input-output dependency. To simplify further computations on
       dependencies, we remove this kind of dependencies from the set of input
       dependencies and keep them only in the set of input-output
       dependencies. *)
    let ins' = Dep_set.diff ins' inouts' in
    {
      (* Initially, a task is represented by a single instruction. *)
      current = [current];
      attrs = attrs;
      ins = ins';
      inouts = inouts';
      children = children;
    }
  (* [Task.has_attr t attr]: checks whether the task [t] has the attribute
     [attr]. *)
  let has_attr (task : t) (attr : TaskAttr.t) : bool =
    TaskAttr_set.mem attr task.attrs
  (* [Task.merge t1 t2]: merges two tasks into a new single task. *)
  let merge (t1 : t) (t2 : t) : t =
    (* For this, we concatenate the lists of associated AST terms, *)
    let current' = t1.current @ t2.current in
    (* compute the union of attributes, *)
    let attrs' = TaskAttr_set.union t1.attrs t2.attrs in
    (* compute the union of both the input dependency set *)
    let ins' = Dep_set.union t1.ins t2.ins in
    (* and the input-output dependency set, *)
    let inouts' = Dep_set.union t1.inouts t2.inouts in
    (* concatenate the list of lists of nested graphs of [t1] and [t2]. *)
    let children' = t1.children @ t2.children in
    {
      current = current';
      attrs = attrs';
      ins = ins';
      inouts = inouts';
      children = children';
    }
  (* [Task.update task instrs]: updates the list of AST terms associated with
     [task] and replaces it with the list of AST terms [instrs]. Other
     components of [task] remain unaffected. *)
  let update (task : t) (instrs : trms) : t = {
      current = instrs;
      attrs = task.attrs;
      ins = task.ins;
      inouts = task.inouts;
      children = task.children;
    }
  (* [Task.empty]: produces an empty task having no AST term associated with it
     as well as without any attributes, dependencies or nested graphs. *)
  let empty () = {
      current = [];
      attrs = TaskAttr_set.empty;
      ins = Dep_set.empty;
      inouts = Dep_set.empty;
      children = [];
    }
  (* [Task.to_string task]: returns a string representation of [task]. *)
  let to_string (task : t) : string =
    let what = List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ " > " ^ desc
                 ) "" task.current in
    let ins = Dep_set.fold
                (fun a acc -> acc ^ " " ^ (Dep.to_string a)) task.ins "" in
    let inouts = Dep_set.fold (
                     fun a acc -> acc ^ " " ^ (Dep.to_string a)
                   ) task.inouts "" in
    what ^ " (in: [" ^ ins ^ " ], inouts: [" ^ inouts ^ " ])\n"
  (* [Task.to_label task]: returns a string representation of [task] used when
     converting a task graph into the Dot text format. See
     [TaskGraphPrinter]. *)
  let to_label (task : t) : string =
    let what = List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ " > " ^ desc
                 ) "" task.current in
    let instr = AstC_to_c.ast_to_string (List.hd task.current) in
    let limit = String.length instr in
    let limit = if limit > 20 then 20 else limit in
    let excerpt = String.sub instr 0 limit in
    what ^ "\n" ^ excerpt
end and TaskGraph : Sig.IM
        with type V.label = Task.t and
             type E.label = TaskWeight.t =
         Imperative.Digraph.AbstractLabeled(Task)(TaskWeight)

(* [TaskGraphPrinter]: a module to convert a [TaskGraph] into a text
   representation such as the Dot format. See [DotExport]. *)
module TaskGraphPrinter = struct
  (* This module actually extends the original [TaskGraph] module. *)
  include TaskGraph
  (* For [TaskGraphPrinter] to work with the export modules, e.g. [DotExport],
     it must implement the following methods. *)
  (* [TaskGraphPrinter.vertex_name vertex]: returns a string representation of
     [vertex] in a [TaskGraph]. *)
  let vertex_name (vertex : V.t) : string =
    (* Get the [Task] corresponding to [vertex]. *)
    let task = TaskGraph.V.label vertex in
    (* Hash [vertex]. *)
    let id = TaskGraph.V.hash vertex in
    (* Determine the kinds of the AST terms associated with the task and *)
    let what = List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ "_" ^ desc
                 ) "" task.current in
    (* append a unique identifier, i.e. the hash of [vertex], to it. *)
    what ^ "_" ^ (string_of_int id)
  (* [TaskGraphPrinter.vertex_attributes vertex]: associates a list of
     attributes to a given [vertex] of a [TaskGraph]. Here, we consider only the
     [Label] attributed, which is produced using the [to_label] method of
     [Task]. *)
  let vertex_attributes
        (vertex : V.t) : Graphviz.DotAttributes.vertex list =
    [ `Label (Task.to_label (TaskGraph.V.label vertex) ) ]
  (* [TaskGraphPrinter.get_nested_graphs vertex]: gather the nested graphs of
     [vertex]. *)
  let get_nested_graphs (vertex : V.t) =
    let task = TaskGraph.V.label vertex in
    List.concat task.children
  (* We leave the following methods without a useful implementation as we do not
     need to use them. However, they still have to be 'implemented'. *)
  let get_subgraph (vertex : V.t) = None
  let graph_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.graph list = []
  let default_vertex_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.vertex list = []
  let default_edge_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.edge list = []
  let edge_attributes
        (edge : E.t) : Graphviz.DotAttributes.edge list = []
end

(* [DotExport]: a module for exporting a [TaskGraph] into the Dot format through
   the [TaskGraphPrinter] module. *)
module DotExport = Graph.Graphviz.Dot(TaskGraphPrinter)

(* [export_task_graph g f]: exports the task graph [g] into the Dot format and
   saves it to the file [f]. *)
let export_task_graph g f : unit =
  let file = open_out f in
  DotExport.output_nested_graph file g;
  close_out file

(* [TaskGraphBuilder]: a module allowing us to make the [TaskGraphOper]
   module. *)
module TaskGraphBuilder = Builder.I(TaskGraph)

(* [TaskGraphOper]: a module providing various graph operations for
   [TaskGraph]s. *)
module TaskGraphOper = Oper.Make(TaskGraphBuilder)

(* [TaskGraphTraverse]: a module implementing a traversal function for task
   graphs. *)
module TaskGraphTraverse : sig
  val codify : (TaskGraph.V.t -> trms) -> TaskGraph.t -> trms
end = struct
  (* [TaskGraphTraverse.H]: a hash table module for task graph vertices. *)
  module H = Hashtbl.Make(TaskGraph.V)
  (* [TaskGraphTraverse.codify c g]: traverses the task graph [g] and codifies
     each vertex into the corresponding output AST term thanks to the
     user-provided codification function [c]. *)
  let codify (c : (TaskGraph.V.t -> trms)) (g : TaskGraph.t) : trms =
    (* Find and extract the root node. *)
    let vs = TaskGraph.fold_vertex (fun v acc ->
                 if TaskGraph.in_degree g v < 1 then v::acc else acc
               ) g [] in
    let r = List.hd vs in
    (* Create an hash table for already visited nodes. *)
    let v = H.create 97 in
    (* Create a queue for elements to visit. *)
    let q = Queue.create () in
    let rec visit (root : bool) (ts : trms) : trms =
      (* If the queue of elements to visit is empty, we are done. Return the
         list of codified elements. *)
      if (Queue.is_empty q) then ts else
        begin
          (* Get the first element from the queue of vertices to visit. This
             will be the current element to process. *)
          let hd = Queue.take q in
          (* Then, retrieve its predecessors and *)
          let p = TaskGraph.pred g hd in
          (* its successors. *)
          let s = TaskGraph.succ g hd in
          (* If all of its predecessors has been visited, i.e. if all of them
             are in the hash table of visited nodes, *)
          let all = List.for_all (fun e -> H.mem v e) p in
          if all && not (H.mem v hd) then
            begin
              (* visit the current element, *)
              H.add v hd ();
              (* push its successors to the queue of vertices to visit, *)
              List.iter (fun e -> Queue.push e q) s;
              (* codify the current element, *)
              let ce = if root then [] else c hd in
              (* append it to the list of already codified elements and
                 recurse. *)
              visit false (ts @ ce)
            end
          (* Otherwise, do nothing and recurse. *)
          else visit false ts
        end
    in
    (* Push the root element to the queue and *)
    Queue.push r q;
    (* Start the codification process with an empty list of codified
       elements. *)
    visit true []
end

