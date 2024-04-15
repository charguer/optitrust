open Ast

(** [task_group_mark]: string used to mark instruction sequences targeted by
    task group insertion. See [Apac_basic.task_group] and
    [Apac_basic.use_goto_for_return]. *)
let task_group_mark : mark = "__apac_task_group"

(** [goto_label]: label used when replacing return statements by gotos within
    the pre-processing stage. See [Apac_basic.use_goto_for_return]. *)
let goto_label : label = "__apac_exit"

(** [result_variable]: name of the variable used to collect return values when
    replacing return statements by gotos within the pre-processing stage. See
    [Apac_basic.use_goto_for_return]. *)
let result_variable : string = "__apac_result"

(** [heapify_mark]: string used to mark sequences for heapification. See
    [Apac_basic.heapify]. *)
let heapify_mark : mark = "__apac_heapify"

(** [heapify_breakable_mark]: string used to mark sequences for heapification.
    This mark is reserved for sequences possibly containing [break] or
    [continue] statements. See [Apac_basic.heapify]. *)
let heapify_breakable_mark : mark = "__apac_heapify_breakable"

(** [profiler_header]: include directive required by the profiler backend. *)
let profiler_header : string = "#include \"apac_profiler.hpp\""

(** [instrument_code]: tells whether the resulting source code should be
    instrumented to enable task granularity control. *)
let instrument_code : bool ref = ref false

(** [apac_count_infinite]: expected name of the optional environment variable
    allowing the end-user to disable the task creation cut-off based on the
    number of active tasks. *)
let apac_count_infinite : string = "APAC_TASK_COUNT_INFINITE"

(** [apac_depth_infinite]: expected name of the optional environment variable
    allowing the end-user to disable the task creation cut-off based on the
    current task depth. *)
let apac_depth_infinite : string = "APAC_TASK_DEPTH_INFINITE"

(** [apac_count_max]: expected name of the optional environment variable
    allowing the end-user to manually set the maximum count of active tasks. *)
let apac_count_max : string = "APAC_TASK_COUNT_MAX"

(** [apac_depth_max]: expected name of the optional environment variable
    allowing the end-user to manually set the maximum task depth. *)
let apac_depth_max : string = "APAC_TASK_DEPTH_MAX"

(** [apac_count_thread_factor]: the default value of [apac_count_max] is a
    multiple of the maximum amount of threads available for parallel task
    execution. This multiple, or this factor, is initially set to [10], but it
    can be changed here. *)
let apac_count_thread_factor : int = 10

(** [apac_depth_max_default]: the default value of [apac_depth_max] is a an
    arbitrary value initially set to [5]. It can be changed here. *)
let apac_depth_max_default : int = 5
