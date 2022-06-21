include Apac_basic

open Ast
open Target


let parallel_task_group ?(mark : mark = "") : Transfo.t =
  iter_on_targets ( fun t p -> 
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    List.iter (fun prg -> transfo_on_targets (trm_add_pragma prg) (target_of_path p)) [Taskgroup; Master; Parallel [] ]
)
