
open Ast
open Target
include Apac_core
include Apac_basic



(* [parallel_task_group ~mark tg]: expects the target [ŧg] to point at a taskable function definition,
    then it will insert  #pragma omp parallel #pragma omp master #pragma omp taskgroup in front of that definition.*)
let parallel_task_group ?(mark : mark = "") : Transfo.t =
  iter_on_targets ( fun t p -> 
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    List.iter (fun prg -> transfo_on_targets (trm_add_pragma prg) (target_of_path p)) [Taskgroup; Master; Parallel [] ]
)

(* let insert_tasks_for_taskable ?(indepth : bool = true) (tsk : taskable) (tg : target) : unit = *)
