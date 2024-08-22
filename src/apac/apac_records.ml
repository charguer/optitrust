open Ast
open Apac_dep
open Apac_tasks
open Apac_const

(** [f]: a function definition record. *)
type f = {
    (** [args]: a list storing the access classification (see [!type:arg]) of
        each function argument. List indices match the positions of the
        arguments in the function definition. *)
    mutable args : arg list;
    (** [graph]: the task candidate graph intermediate representation of the
        function (see [!module:TaskGraph]). *)
    mutable graph : TaskGraph.t;
    (** [scope]: a hash table of function-local variables, including the
        arguments, and their number of levels of indirection. For example, in
        the case of the following function definition,

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
    scope : int Var_Hashtbl.t;
    (** [ast]: a copy of the original abstract syntax tree intermediate
        representation of the function (see [!type:trm]). *)
    ast : trm
  }
(** [arg]: a function argument access classification. *)
and arg =
  (** The function {b does not alter} the argument by side-effect. *)
  | Read
  (** The function {b does alter} the argument by side-effect. *)
  | ReadWrite

(** [functions]: a hash table of function definition records with an initial
    size of 10 entries (see [!module:Var_Hashtbl]). *)
let functions : f Var_Hashtbl.t = Var_Hashtbl.create 10

(** [const_records]: hash table of [const_fun] with an initial size of 10. The
    size of the table will grow automatically if needed. *)
let const_records : const_funs = Var_Hashtbl.create 10

(** [const_candidates]: hash table of functions identified as candidates for
    taskification with an initial size of 10. The size of the table will grow
    automatically if needed. *)
let const_candidates : unit Var_Hashtbl.t = Var_Hashtbl.create 10

(** [mutables]: map of dependencies on mutable, according to the OptiTrust
    definition, pointer variables to copies of themselves wrapped with a
    [Trm.trm_get]. We need these copies to ensure that dependencies involving
    mutable pointer variables appear correctly formatted with OpenMP pragmas.
    However, this can be done only at the very last moment, i.e. when building
    the pragmas in [Apac_backend.emit_omp_task]. Otherwise, it leads to
    different [Dep] expressions and may impact the dependency-related
    operations. This map allows us to differ the special encoding until the
    transformation of program's task graph into a new AST. *)
let mutables : Dep.t Dep_map.t ref = ref Dep_map.empty
