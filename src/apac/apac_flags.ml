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

(** [omit]: a pattern of names of functions to omit entirely during the
    compilation process. *)
let omit : string ref = ref ""

(** [skip]: a set of names of functions to exclude from the selection of
    taskification candidates (see [!Apac_preprocessing.select_candidates]). *)
let skip : Tools.String_set.t ref = ref Tools.String_set.empty

(** [constify]: toggles constification of [input]. *)
let constify : bool ref = ref false

(** [constify_quietly]: toggles the application of the constification results on
    the [input] source code. *)
let constify_quietly : bool ref = ref false

(** {1 Task candidate discovery and optimization stage} *)

(** [keep_graphs]: tells whether we should dump task candidate graphs to Dot and
    subsequently to Pdf files at each transformation. *)
let keep_graphs : bool ref = ref false

(** [keep_graphs_in]: name of a folder in the current working directory to keep
    the task candidate graphs in (see [!Apac_miscellaneous.gwd]). *)
let keep_graphs_in : string ref = ref "apac_task_candidate_graphs"

(** [traversal]: name of the algorithmic variant for traversing task candidate
    graphs (see [!module:Apac_tasks.TaskGraphTraverse]). *)
let traversal : string ref = ref "strict"

(** {1 Instrumented code generation and run-time analysis} *)

(** {2 Profiling} *)

(** [profile]: toggles profiling of the input source code. *)
let profile : bool ref = ref false

(** [profile_with]: arguments to pass to the profiling executable. *)
let profile_with : string ref = ref ""

(** [profile_with_custom]: custom command line for running the profiling
    executable. *)
let profile_with_custom : string ref = ref ""

(** [compiler]: enumeration of C/C++ compilers we can choose from to build the
    profiling executable. *)
type compiler = Gnu | Clang | Custom

(** [compile_with]: the compiler and the compilation options we use to build the
    profiling executable. *)
let compile_with : (compiler * string) ref =
  ref (Clang, "-Wall -Wno-unused-label")

(** {2 Modeling} *)

(** [model_with]: additional arguments to pass to the execution time modeler. *)
let model_with : string ref = ref ""

(** {1 Parallel code generation} *)

(** [main]: name of the function in the input source code we consider as the
    taskification starting point, i.e. the one we put the initial task group in.
    By default, it is the `main' function. *)
let main : string ref = ref "main"

(** [cutoff_count]: toggles task granularity control based on a global task
    submission counter (see [ApacCount] in [!type:apac_variable]). See also
    [!Apac_parallelization.cutoff_count_and_depth] and
    [!Apac_parallelization.codegen_openmp]. *)
let cutoff_count : bool ref = ref false

(** [cutoff_depth]: toggles task granularity control based on a per-thread
    parallelism depth counter (see [ApacDepth] in [!type:apac_variable]). See
    also [!Apac_parallelization.cutoff_count_and_depth] and
    [!Apac_parallelization.codegen_openmp]. *)
let cutoff_depth : bool ref = ref false

(** [count_max_thread_factor]: this value times the number of threads available
    for parallel execution gives the maximum amount of submitted tasks (see
    [ApacCountMax] in type [!type:apac_variable]). The user can either adjust
    this factor or fix an absolute limit thanks to the [!Apac_macros.count_max]
    environment variable. *)
let count_max_thread_factor : int ref = ref 10

(** [depth_max_default]: default task depth maximum (see [ApacDepthMax] in type
    [!type:apac_variable]). *)
let depth_max_default : int ref = ref 5

(** [sequential]: in the resulting parallel source code, the task granularity
    control mechanism relying on a per-thread parallelism depth counter switches
    to the sequential implementation of a given parallel function when the
    counter reaches [ApacDepthMax]. To this end, the compiler takes care of
    preserving a sequential implementation of each function it parallelizes in
    the output source code. However, if the input source code already contains a
    copy of each function to parallelize the user excludes from the compilation
    process using [!omit], it is possible to use these copies instead of
    generating new ones. In order to allow the compiler to identify an existing
    sequential implementation of a function, the user should provide an adequate
    [sequential] function name pattern where [!Apac_macros.depth_placeholder]
    represents the function name, e.g. if [!Apac_macros.depth_placeholder] is
    `@f' (default value), then `"@f_seq$"' means that the names of the
    sequential implementations end with `_seq'. *)
let sequential : string ref = ref ""


