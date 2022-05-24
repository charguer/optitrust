open Ast

(******************************************************************************)
(*                             OpenMP routines                                *)
(******************************************************************************)
let set_num_threads_aux (nb_threads : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_num_threads nb_threads)) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_num_threads_aux: expected the sequence where the call to the routine is going to be added"


let set_num_threads (nb_threads : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (set_num_threads_aux nb_threads index)


let get_num_threads_aux (nb_threads : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl nb_threads in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var nb_threads) (trm_omp_routine (Get_num_threads))
    | None ->
      trm_let_mut (nb_threads, typ_int()) (trm_omp_routine (Get_num_threads))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_num_threads_aux: expected the sequence where the call to the routine is going to be added"

let get_num_threads (nb_threads : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_threads_aux nb_threads index)

let declare_num_threads_aux (nb_threads : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_dl = trm_let_mut (nb_threads, typ_int()) (trm_uninitialized()) in
    let new_tl = Mlist.insert_at index new_dl tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.declare_num_threads_aux: expected the sequence where the call to the routine is going to be added"


let declare_num_threads (nb_threads : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (declare_num_threads_aux nb_threads index)



let get_max_threads_aux (max_threads : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl max_threads in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var max_threads) (trm_omp_routine (Get_max_threads))
    | None ->
      trm_let_mut (max_threads, typ_int()) (trm_omp_routine (Get_max_threads))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_max_threads_aux: expected the sequence where the call to the routine is going to be added"

let get_max_threads (max_threads : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_max_threads_aux max_threads index)


let get_thread_num_aux (const : bool) (thread_num : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.local_decl thread_num t in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var thread_num) (trm_omp_routine (Get_thread_num))
    | None ->
      if const
        then trm_let_immut (thread_num, typ_int()) (trm_omp_routine (Get_thread_num))
        else trm_let_mut (thread_num, typ_int()) (trm_omp_routine (Get_thread_num))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_thread_num_aux: expected the sequence where the call to the routine is going to be added"

let get_thread_num (const : bool) (thread_num : var) (index : int) (t : trm) (p : Path.path) : trm =
  Target.apply_on_path (get_thread_num_aux const thread_num index) t  p

let get_num_procs_aux (num_procs : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl num_procs in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var num_procs) (trm_omp_routine (Get_num_procs))
    | None ->
      trm_let_mut (num_procs, typ_int()) (trm_omp_routine (Get_num_procs))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_num_procs_aux: expected the sequence where the call to the routine is going to be added"

let get_num_procs (num_procs : var) (index : int) (t : trm) (p : Path.path) : trm =
  Target.apply_on_path (get_num_procs_aux num_procs index) t p


let in_parallel_aux (in_parallel : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl in_parallel in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var in_parallel) (trm_omp_routine (In_parallel))
    | None ->
      trm_let_mut (in_parallel, typ_int()) (trm_omp_routine (In_parallel))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.in_parallel_aux: expected the sequence where the call to the routine is going to be added"

let in_parallel (in_parallel : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (in_parallel_aux in_parallel index)


let set_dynamic_aux (thread_id : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_dynamic thread_id)) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_dynamic_aux: expected the sequence where the call to the routine is going to be added"

let set_dynamic (thread_id : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (set_dynamic_aux thread_id index)


let get_dynamic_aux (is_dynamic : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl is_dynamic in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var is_dynamic) (trm_omp_routine (Get_dynamic))
    | None ->
      trm_let_mut (is_dynamic, typ_int()) (trm_omp_routine (Get_dynamic))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_dynamic_aux: expected the sequence where the call to the routine is going to be added"

let get_dynamic (is_dynamic : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_dynamic_aux is_dynamic index)


let get_cancellation_aux (is_cancellation : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl is_cancellation in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var is_cancellation) (trm_omp_routine (Get_cancellation))
    | None ->
      trm_let_mut (is_cancellation, typ_int()) (trm_omp_routine (Get_cancellation))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_cancellation_aux: expected the sequence where the call to the routine is going to be added"

let get_cancellation (is_cancellation : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_cancellation_aux is_cancellation index)

let set_nested_aux (nested : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_nested nested)) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_nested_aux: expected the sequence where the call to the routine is going to be added"

let set_nested (nested : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (set_nested_aux nested index)

let get_nested_aux (is_nested : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl is_nested in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var is_nested) (trm_omp_routine (Get_nested))
    | None ->
      trm_let_mut (is_nested, typ_int()) (trm_omp_routine (Get_nested))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_nested_aux: expected the sequence where the call to the routine is going to be added"

let get_nested (is_nested : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_nested_aux is_nested index)


let set_schedule_aux (sched_kind : sched_type) (modifier : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_schedule (sched_kind, modifier))) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_nested_aux: expected the sequence where the call to the routine is going to be added"

let set_schedule (sched_kind : sched_type) (modifier : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (set_schedule_aux sched_kind modifier index)


let get_schedule_aux (sched_kind : sched_type) (modifier : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Get_schedule (sched_kind, modifier))) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_nested_aux: expected the sequence where the call to the routine is going to be added"

let get_schedule (sched_kind : sched_type) (modifier : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_schedule_aux sched_kind modifier index)

let get_thread_limit_aux (limit : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl limit in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var limit) (trm_omp_routine (Get_thread_limit))
    | None ->
      trm_let_mut (limit, typ_int()) (trm_omp_routine (Get_thread_limit))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_thread_limit_aux: expected the sequence where the call to the routine is going to be added"

let get_thread_limit (limit : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_thread_limit_aux limit index)

let set_max_active_levels_aux (max_levels : int) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_max_active_levels max_levels)) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_max_active_levels_aux: expected the sequence where the call to the routine is going to be added"

let set_max_active_levels (max_levels : int) (index : int) : Target.Transfo.local =
  Target.apply_on_path (set_max_active_levels_aux max_levels index)


let get_max_active_levels_aux (max_levels : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl max_levels in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var max_levels) (trm_omp_routine (Get_max_active_levels))
    | None ->
      trm_let_mut (max_levels, typ_int()) (trm_omp_routine (Get_max_active_levels))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_max_active_levels_aux: expected the sequence where the call to the routine is going to be added"

let get_max_active_levels (max_levels : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_max_active_levels_aux max_levels index)

let get_level_aux (level : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl level in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var level) (trm_omp_routine (Get_level))
    | None ->
      trm_let_mut (level, typ_int()) (trm_omp_routine (Get_level))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_level_aux: expected the sequence where the call to the routine is going to be added"

let get_level (level : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_level_aux level index)

let get_ancestor_thread_num_aux (thread_num : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl thread_num in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var thread_num) (trm_omp_routine (Get_ancestor_thread_num))
    | None ->
      trm_let_mut (thread_num, typ_int()) (trm_omp_routine (Get_thread_num))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_ancestor_thread_num_aux: expected the sequence where the call to the routine is going to be added"

let get_ancestor_thread_num (thread_num : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_ancestor_thread_num_aux thread_num index)

let get_team_size_aux (level : int) (size : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl size in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var size) (trm_omp_routine (Get_team_size level))
    | None ->
      trm_let_mut (size, typ_int()) (trm_omp_routine (Get_team_size level))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_team_size_aux: expected the sequence where the call to the routine is going to be added"

let get_team_size (level : int) (size : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_team_size_aux level size index)

let get_active_level_aux (active_level : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl active_level in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var active_level) (trm_omp_routine (Get_active_level))
    | None ->
      trm_let_mut (active_level, typ_int()) (trm_omp_routine (Get_active_level))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_active_level_aux: expected the sequence where the call to the routine is going to be added"

let get_active_level (active_level : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_active_level_aux active_level index)

let in_final_aux (in_final : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl in_final in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var in_final) (trm_omp_routine (In_final))
    | None ->
      trm_let_mut (in_final, typ_int()) (trm_omp_routine In_final)
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.in_final_aux: expected the sequence where the call to the routine is going to be added"

let in_final (in_final : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (in_final_aux in_final index)

let get_proc_bind_aux (proc_bind : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl proc_bind in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var proc_bind) (trm_omp_routine (Get_proc_bind))
    | None ->
      trm_let_mut (proc_bind, typ_int()) (trm_omp_routine (Get_proc_bind))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_proc_bind_aux: expected the sequence where the call to the routine is going to be added"

let get_proc_bind (proc_bind : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_proc_bind_aux proc_bind index)

let set_default_device_aux (device_num : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_default_device device_num)) tl in
    trm_seq ~annot:t.annot new_tl

  | _ -> fail t.loc "Omp_core.set_default_device_aux: expected the sequence where the call to the routine is going to be added"

let set_default_device (device_num : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (set_default_device_aux device_num index)

let get_default_device_aux (default_device : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl default_device in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var default_device) (trm_omp_routine (Get_default_device))
    | None ->
      trm_let_mut (default_device, typ_int()) (trm_omp_routine (Get_default_device))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_default_device_aux: expected the sequence where the call to the routine is going to be added"

let get_default_device (default_device : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_default_device_aux default_device index)


let get_num_devices_aux (num_devices : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl num_devices in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var num_devices) (trm_omp_routine (Get_num_devices))
    | None ->
      trm_let_mut (num_devices, typ_int()) (trm_omp_routine (Get_num_devices))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_num_devices_aux: expected the sequence where the call to the routine is going to be added"

let get_num_devices (num_devices : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_devices_aux num_devices index)

let get_num_teams_aux (num_teams : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl num_teams in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var num_teams) (trm_omp_routine (Get_num_teams))
    | None ->
      trm_let_mut (num_teams, typ_int()) (trm_omp_routine (Get_num_teams))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_num_teams_aux: expected the sequence where the call to the routine is going to be added"

let get_num_teams (num_teams : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_num_teams_aux num_teams index)

let get_team_num_aux (team_num : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl team_num in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var team_num) (trm_omp_routine (Get_team_num))
    | None ->
      trm_let_mut (team_num, typ_int()) (trm_omp_routine (Get_team_num))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_team_num_aux: expected the sequence where the call to the routine is going to be added"

let get_team_num (team_num : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_team_num_aux team_num index)

let is_initial_device_aux (is_initial_device : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl is_initial_device in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var is_initial_device) (trm_omp_routine (In_final))
    | None ->
      trm_let_mut (is_initial_device, typ_int()) (trm_omp_routine (In_final))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.is_initial_device_aux: expected the sequence where the call to the routine is going to be added"

let is_initial_device (is_initial_device : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (is_initial_device_aux is_initial_device index)

let init_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Init_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.init_lock_aux: expected the sequence where the call to the routine is going to be added"

let init_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (init_lock_aux lock index)

let init_nest_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Init_nest_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.init_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let init_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (init_nest_lock_aux lock index)


let destroy_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Destroy_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.destroy_lock_aux: expected the sequence where the call to the routine is going to be added"

let destroy_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (destroy_lock_aux lock index)

let destroy_nest_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Destroy_nest_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.destroy_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let destroy_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (destroy_nest_lock_aux lock index)


let set_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.set_lock_aux: expected the sequence where the call to the routine is going to be added"

let set_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (set_lock_aux lock index)

let set_nest_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_nest_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.set_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let set_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (set_nest_lock_aux lock index)


let unset_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Unset_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.unset_lock_aux: expected the sequence where the call to the routine is going to be added"

let unset_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (unset_lock_aux lock index)

let unset_nest_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Unset_nest_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.unset_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let unset_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (unset_nest_lock_aux lock index)

let test_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.test_lock_aux: expected the sequence where the call to the routine is going to be added"

let test_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (test_lock_aux lock index)

let test_nest_lock_aux (lock : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_at index (trm_omp_routine (Set_nest_lock lock)) tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.test_nest_lock_aux: expected the sequence where the call to the routine is going to be added"

let test_nest_lock (lock : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (test_nest_lock_aux lock index)

let get_wtime_aux (wtime : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl wtime in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var wtime) (trm_omp_routine (Get_wtime))
    | None ->
      trm_let_mut (wtime, typ_int()) (trm_omp_routine (Get_wtime))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_wtime_aux: expected the sequence where the call to the routine is going to be added"

let get_wtime (wtime : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_wtime_aux wtime index)

let get_wtick_aux (wtick : var) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let find_prev_decl = Internal.toplevel_decl wtick in
    let new_trm =
    begin match find_prev_decl with
    | Some _ ->
      trm_set (trm_var wtick) (trm_omp_routine (Get_wtick))
    | None ->
      trm_let_mut (wtick, typ_int()) (trm_omp_routine (Get_wtick))
    end in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Omp_core.get_wtick_aux: expected the sequence where the call to the routine is going to be added"

let get_wtick (wtick : var) (index : int) : Target.Transfo.local =
  Target.apply_on_path (get_wtick_aux wtick index)
