open Ast
open Graph

(****************************)
(* INSTRUCTION DEPENDENCIES *)
(****************************)

(* The [Ast] module defines a dependency type -- [dep]. Originally, the later is
   used to represent dependencies between OpenMP tasks in the OpenMP [depend]
   directives. In APAC, we use this type to represent inter-task data
   dependencies in general, not necessarily in the OpenMP context. Thus, in this
   section, we extend the implementation of the dependency [dep] type. We then
   use the latter in our task graph implementation (see below). *)

(* [dep_to_string d]: generates a string representation of the dependency
   [d]. See the [Ast.dep] data type. *)
let rec dep_to_string (d : dep) : string =
  match d with
  | Dep_ptr d' ->
     (*"*" ^*) dep_to_string d'
  | Dep_var v -> v.name
  | Dep_trm (t, v) -> v.name
     (*AstC_to_c.ast_to_string t*)

(* [dep_get_atomic d]: if [d] is a dependency represented by a pointer
   expression, e.g. [*c], the function returns the base dependency element, i.e.
   the underlying variable [Dep_var] or term [Dep_trm]. If the degree of the
   pointer expression is greater than 1, it will iterate over the nested pointer
   expression until it reaches the underlying [Dep_var] or [Dep_trm]. *)
let rec dep_get_atomic (d : dep) : dep =
  match d with
  | Dep_ptr d' -> dep_get_atomic d'
  | _ -> d

(* [Dep]: a module to represent instruction dependencies. See the [Ast.dep] data
   type. *)
module Dep = struct
  type t = dep
  (* In our case, dependency expressions are not only simple variables. We
     consider also pointer expression, e.g. [*c], as well as more complex terms,
     e.g. [tab[0]]. Therefore, to compare two dependencies, we compare their
     string representation, see [dep_to_string]. *)
  let compare d1 d2 =
    let d1' = dep_to_string d1 in
    let d2' = dep_to_string d2 in
    String.compare d1' d2'
  let equal d1 d2 =
    let d1' = dep_to_string d1 in
    let d2' = dep_to_string d2 in
    d1' = d2'
end

(* [Dep_set]: a module to represent sets of dependencies. Typically, we will use
   a set of input (read-only) dependencies as well as a set of input-output
   (read-write) dependencies. *)
module Dep_set = Set.Make(Dep)

(* [dep_attr]: enumeration of data dependency attributes (see
   [trm_discover_dependencies]). *)
type dep_attr =
  | Regular (* default value, no attribute *)
  | FunArgIn (* function call argument, read-only dependency *)
  | FunArgInOut (* function call argument, read-write dependency *)

(* [dep_set_of_stack s]: converts the stack of dependencies [s] into a
   dependency set. *)
let dep_set_of_stack s = Dep_set.of_seq (Stack.to_seq s)

(* [dep_set_to_list s]: converts the dependency set [s] into a list of
   dependencies. *)
let dep_set_to_list s = List.of_seq (Dep_set.to_seq s)

(* [var_to_dep v degree]: converts the variable [v] into a data dependency. If
   [degree] is greater than 0, the resulting [Dep_var] will be wrapped into
   [degree] pointer expressions [Dep_ptr]. *)
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

(* [trm_to_dep t v degree]: converts the term [t] into a data dependency. Note
   that a dependency term [Dep_trm] is composed of the term itself, e.g.
   [tab[0]], but also of the variable involved in the data access, e.g. [tab].
   Moreover, if [degree] is greater than 0, the resulting [Dep_trm] will be
   wrapped into [degree] pointer expressions [Dep_ptr]. *)
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

(******************)
(* GRAPH OF TASKS *)
(******************)

(* [task_backend]: enumeration of supported task runtimes. *)
type task_backend =
  | OpenMP (* Emit OpenMP tasks. *)
  | ApacProfiler (* Do not create tasks. Insert calls to profiling functions
                    instead. *)

(* Automatic taskification involves the creation of a task graph correspoding to
   the input source code. We create one task graph per program scope. This way,
   to a function body containing, for example, a for-loop, we will associate one
   task graph representing the instructions of the function's body. A second
   task graph will be associated to the node of the first graph representing the
   for-loop instruction. This second task graph will reflect the instruction of
   the for-loop body (within the encompassing function's body).

   To model and manipulate task graphs, we rely on the Ocamlgraph library.

   For starters, we need to define the shape of graph nodes. *) 

module TaskWeight : sig
  type t = int
  val default : t
  val compare : t -> t -> int
end = struct
  type t = int
  let default = 1
  let compare tw1 tw2 = tw1 - tw2
end

(* [node]: a graph node type. The initial task graph of a function considers
   each instruction and each block of instructions (loops, scopes, switches,
   ...) as a separate task. While building the initial task graph of a function,
   we have to compute the potential data dependencies between the future tasks.
   During this process, we iterate over the local AST of the target function,
   look for data dependencies, construct the output graph nodes and add edges
   between the nodes based on the discovered dependencies. Each [node] can
   represent a task by itself. A [node] element is composed of: *)

module rec Task : sig
         type t = {
             current : trms;
             last : bool;
             mutable wait : bool;
             mutable ins : Dep_set.t;
             mutable inouts : Dep_set.t;
             children : TaskGraph.t list list;
           }
         val create :
           trm -> Var_set.t -> Dep_set.t -> Dep_set.t ->
           TaskGraph.t list list -> t
         val wait :
           trm -> Var_set.t -> Dep_set.t -> Dep_set.t ->
           TaskGraph.t list list -> t
         val last : trm  -> t
         val merge : t -> t -> t
         val update : t -> trms -> t
         val empty : trm -> t
         val to_string : t -> string
         val to_label : t -> string
       end = struct
  type t = {
             current : trms;
             last : bool;
             mutable wait : bool;
             mutable ins : Dep_set.t;
             mutable inouts : Dep_set.t;
             children : TaskGraph.t list list;
    }
  let create (current : trm) (scope : Var_set.t)
        (ins : Dep_set.t) (inouts : Dep_set.t)
        (children : TaskGraph.t list list) : t =
    let ins' = Dep_set.filter (
                   fun d -> match (dep_get_atomic d) with
                            | Dep_var v -> Var_set.mem v scope
                            | Dep_trm (_, v) -> Var_set.mem v scope
                            | _ -> false
                 ) ins in
    let inouts' = Dep_set.filter (
                      fun d -> match (dep_get_atomic d) with
                               | Dep_var v -> Var_set.mem v scope
                               | Dep_trm (_, v) -> Var_set.mem v scope
                               | _ -> false
                    ) inouts in
    let ins' = Dep_set.diff ins' inouts' in
    {
      current = [current];
      last = false;
      wait = false;
      ins = ins';
      inouts = inouts';
      children = children;
    }
  let wait (current : trm) (scope : Var_set.t)
        (ins : Dep_set.t) (inouts : Dep_set.t)
        (children : TaskGraph.t list list) : t =
    let task = create current scope ins inouts children in
    task.wait <- true;
    task
  let last (current : trm) : t = {
      current = [current];
      last = true;
      wait = true;
      ins = Dep_set.empty;
      inouts = Dep_set.empty;
      children = [];
    }
  let merge (t1 : t) (t2 : t) : t =
    let current' = t1.current @ t2.current in
    let ins' = Dep_set.union t1.ins t2.ins in
    let inouts' = Dep_set.union t1.inouts t2.inouts in
    let children' = List.append t1.children t2.children in
    {
      current = current';
      last = t1.last || t2.last;
      wait = t1.wait || t2.wait;
      ins = ins';
      inouts = inouts';
      children = children';
    }
  let update (task : t) (instrs : trms) : t = {
      current = instrs;
      last = task.last;
      wait = task.wait;
      ins = task.ins;
      inouts = task.inouts;
      children = task.children;
    }
  let empty (current : trm) = {
      current = [current];
      last = false;
      wait = false;
      ins = Dep_set.empty;
      inouts = Dep_set.empty;
      children = [];
    }
  let to_string (task : t) : string =
    let what = List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ " > " ^ desc
                 ) "" task.current in
    let ins = Dep_set.fold
                (fun a acc -> acc ^ " " ^ (dep_to_string a)) task.ins "" in
    let inouts = Dep_set.fold (
                     fun a acc -> acc ^ " " ^ (dep_to_string a)
                   ) task.inouts "" in
    what ^ " (in: [" ^ ins ^ " ], inouts: [" ^ inouts ^ " ])\n"
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
end and TaskGraph : Sig.IM with type V.label = Task.t and type E.label = TaskWeight.t = Imperative.Digraph.AbstractLabeled(Task)(TaskWeight)

let fresh_vertex_id = Tools.fresh_generator ()

module TaskGraphPrinter = struct
  include TaskGraph
  let vertex_name (vertex : V.t) : string =
    let task = TaskGraph.V.label vertex in
    let id = TaskGraph.V.hash vertex in
    let what = List.fold_left (fun acc term ->
                   let desc = trm_desc_to_string term.desc in
                   acc ^ "_" ^ desc
                 ) "" task.current in
    what ^ "_" ^ (string_of_int id)
  let get_subgraph (vertex : V.t) = None
  let get_nested_graphs (vertex : V.t) =
    let task = TaskGraph.V.label vertex in
    List.concat task.children
  let graph_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.graph list = []
  let default_vertex_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.vertex list = []
  let vertex_attributes
        (vertex : V.t) : Graphviz.DotAttributes.vertex list =
    [ `Label (Task.to_label (TaskGraph.V.label vertex) ) ]
  let default_edge_attributes
        (graph : TaskGraph.t) : Graphviz.DotAttributes.edge list = []
  let edge_attributes
        (edge : E.t) : Graphviz.DotAttributes.edge list = []
end

module DotExport = Graph.Graphviz.Dot(TaskGraphPrinter)

let export_task_graph g f : unit =
  let file = open_out f in
  DotExport.output_nested_graph file g;
  close_out file

module TaskGraphBuilder = Builder.I(TaskGraph)

module TaskGraphOper = Oper.Make(TaskGraphBuilder)

module TaskGraphTraverse : sig
  val codify : (TaskGraph.V.t -> trms) -> TaskGraph.t -> trms
end = struct
  module H = Hashtbl.Make(TaskGraph.V)

  let codify (c : (TaskGraph.V.t -> trms)) (g : TaskGraph.t) : trms =
    let vs = TaskGraph.fold_vertex (fun v acc ->
                 if TaskGraph.in_degree g v < 1 then v::acc else acc
               ) g [] in
    let r = List.hd vs in
    let v = H.create 97 in
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
    
