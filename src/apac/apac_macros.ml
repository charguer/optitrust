open Ast

(* [task_group_mark]: string used to mark instruction sequences targeted by task
   group insertion. See [Apac_basic.task_group] and
   [Apac_basic.use_goto_for_return]. *)
let task_group_mark : mark = "__apac_task_group"

(* [goto_label]: label used when replacing return statements by gotos within
   the pre-processing stage. See [Apac_basic.use_goto_for_return]. *)
let goto_label : label = "__apac_exit"

(* [heapify_mark]: string used to mark sequences for heapification. See
   [Apac_basic.heapify]. *)
let heapify_mark : mark = "__apac_heapify"

(* [heapify_breakable_mark]: string used to mark sequences for heapification.
   This mark is reserved for sequences possibly containing [break] or [continue]
   statements. See [Apac_basic.heapify]. *)
let heapify_breakable_mark : mark = "__apac_heapify_breakable"
