open Ast
open Apac_dep
open Apac_const

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
