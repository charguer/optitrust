(** {0:flags Flags}

    This module provides the declarations and default values of parameters the
    user can alter using command-line options or environment variables. *)

(** {1 Generic parameters} *)

(** [input]: path to the input source code. *)
let input : string ref = ref ""

(** [output]: path to the output source code. *)
let output : string ref = ref ""

(** [verbose]: toggles verbose output for debugging purposes. *)
let verbose : bool ref = ref false

(** {1 Pre-processing stage} *)

(** [main]: name of the function in the input source code we consider as the
    taskification starting point, i.e. the one we put the initial task group in.
    By default, it is the `main' function. *)
let main : string ref = ref "main"

(** {1 Task candidate discovery and optimization stage} *)

(** [constify]: toggles constification of [input]. *)
let constify : bool ref = ref false

(** [traversal]: name of the algorithmic variant for traversing task candidate
    graphs (see [!module:TaskGraphTraverse]). *)
let traversal : string ref = ref "strict"

(** [keep_graphs]: tells whether we should dump task candidate graphs to Dot and
    subsequently to Pdf files at each transformation. *)
let keep_graphs : bool ref = ref false

(** [keep_graphs_in]: name of a folder in the current working directory to keep
    the task candidate graphs in (see [!Apac_miscellaneous.gwd]). *)
let keep_graphs_in : string ref = ref "apac_task_candidate_graphs"

(** {1 Instrumented code generation and run-time analysis} *)

(** {2 Profiling} *)

(** [profile]: toggles profiling of the input source code. *)
let profile : bool ref = ref false

(** [profile_with]: arguments to pass to the profiling executable. *)
let profile_with : string ref = ref ""

(** [compiler]: enumeration of C/C++ compilers we can choose from to build the
    profiling executable. *)
type compiler = Gnu | Clang

(** [compile_with]: the compiler and the compilation options we use to build the
    profiling executable. *)
let compile_with : (compiler * string) ref =
  ref (Gnu, "-Wall -Wno-unused-label")

(** {2 Modeling} *)

(** [model_with]: additional arguments to pass to the execution time modeler. *)
let model_with : string ref = ref ""

(** {1 Parallel code generation} *)

(** [instrument]: toggles output source code instrumentation for task depth and
    granularity control according to global counters (see [ApacDepth] and
    [ApacCount] in type [!type:apac_variable]). *)
let instrument : bool ref = ref false

(** [count_max_thread_factor]: this value times the number of threads available
    for parallel execution gives the maximum amount of submitted tasks (see
    [ApacCountMax] in type [!type:apac_variable]). The user can either adjust
    this factor or fix an absolute limit thanks to the [!Apac_macros.count_max]
    environment variable. *)
let count_max_thread_factor : int ref = ref 10

(** [depth_max_default]: default task depth maximum (see [ApacDepthMax] in type
    [!type:apac_variable]). *)
let depth_max_default : int ref = ref 5


