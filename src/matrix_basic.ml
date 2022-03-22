open Ast

(* [intro_mcalloc tg] expects the target [tg] pointing to a call to funciton alloc
      then it will replace this call with a call to MCALLOC
*)
let intro_mcalloc : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.intro_mcalloc)

(* [intro_mmalloc tg] expects the target [tg] pointing to a call to funciton malloc
      then it will replace this call with a call to MMALLOC
*)
let intro_mmalloc : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.intro_mmalloc)

(* [intro_mindex dim tg] expects the target [tg] pointing to an array access
    then it will replace that access to let say index i with an access at
    MINDEX (dim,i)
*)
let intro_mindex (dim : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.intro_mindex dim)

(* [reorder_dims order tg]: expects the target [tg] pointing to a call to ALLOC functions, or MINDEX
      then it will reorder their args based on [order], where [order] is a list of indices which the
      current args should follow
*)
let reorder_dims ?(rotate_n : int = 0) ?(order : int list = [])  (): Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.reorder_dims rotate_n order)

(* [insert_alloc_dim new_dim]: expects the target [tg] pointing to call to ALLOC functions, then it will
      add a new arg at the begining of the list of args in the targeted call
 *)
let insert_alloc_dim (new_dim : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.insert_alloc_dim new_dim)

(* [insert_access_dim new_dim new_index tg] expects the target [tg] pointing to an array access, then it will
    add two new args in the call to MINDEX function inside that array access *)

let insert_access_dim_index (new_dim : trm) (new_index : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.insert_access_dim_index new_dim new_index)

(* [biject fun_name tg] expectes the target [tg] poiting to a function call then it
      replaces the name of that function call with [fun_name]
*)
let biject (fun_name : string) : Target.Transfo.t =
  Expr.replace_fun fun_name

(* [local_name ~mark var into tg] expects the target pointing to an instruction that contains
      an occurrence of [var] then it will define a matrix [into] whose dimensions will be the same
      as the one of [var]. Then we copy the contents of the matrix [var] into [into] and finaly we
      free up the memory.
 *)

let local_name ?(my_mark : mark option) ?(indices : (var list) = []) ?(alloc_instr : Target.target option = None) (v : var) ~into:(into : var) ?(local_ops : local_ops = Local_arith (Lit_int 0, Binop_add)) (tg : Target.target) : unit =
  let remove = (my_mark = None) in 
  let get_alloc_type_and_trms (t : trm) (tg1 : Target.target) : typ * (trms * trm * bool) = 
    let var_type = begin match t.desc with 
      | Trm_let (_, (_, ty), _) -> get_inner_ptr_type ty
      | Trm_apps (_, [lhs; _rhs]) when is_set_operation t -> 
        begin match lhs.typ with 
        | Some ty -> ty
        | None -> fail t.loc (Printf.sprintf "get_alloc_type_and_trms: couldn't findd the type of variable %s\n'" v)
        end
      | _ -> fail t.loc (Printf.sprintf "get_alloc_type_and_trms: couldn't findd the type of variable %s, alloc_instr 
          target doesn't point to a write operation or a variable declaration \n'" v)
      end in 
      let alloc_trms = begin match Target.get_trm_at (tg1 @ [Target.cFun ~regexp:true "M.ALLOC."]) with 
        | Some at -> 
          begin match Matrix_core.alloc_inv at with 
          | Some (dims, sz, zero_init) -> (dims, sz, zero_init) 
          | _ -> fail t.loc "get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
          end
        | None -> fail None "get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
        end in (var_type, alloc_trms)
    in
  Internal.nobrace_remove_after ~remove (fun _ -> 
    Target.(apply_on_targets (fun t p -> 
      let seq_p, _ = Internal.isolate_last_dir_in_seq p in
      let seq_tg = Target.target_of_path seq_p in 
      let var_target = cOr [[cVarDef v]; [cWriteVar v]] in 
      begin match alloc_instr with 
      | Some tg1 -> 
        begin match get_trm_at tg1 with 
        | Some t1 ->
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in 
          if not remove then Internal.nobrace_enter();
          Matrix_core.local_name my_mark v into alloc_trms var_type indices local_ops t p
        | None -> fail None "local_name: alloc_instr target does not match to any ast node"
        end
      | None -> 
        begin match get_trm_at (seq_tg @ [var_target]) with 
        | Some t1 ->
          let tg1 = (seq_tg @ [var_target]) in
          let var_type, alloc_trms = get_alloc_type_and_trms t1 tg1 in 
          if not remove then Internal.nobrace_enter();
          Matrix_core.local_name my_mark v into alloc_trms var_type indices local_ops t p
          
        | None -> fail None "local_name: alloc_instr target does not match to any ast node"
        end
      end
    ) tg)
  
  
  )

(* [delocalize ~init_zero ~acc_in_place ~acc ~dim ~index ~ops] a generalized version of variable_delocalize*)
let delocalize ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option) ?(any_mark : mark = "") ?(labels : label list = []) ~dim:(dim : trm)  ~index:(index : string) ~ops:(dl_o : local_ops) : Target.Transfo.t =
    Target.apply_on_targets (Matrix_core.delocalize dim init_zero acc_in_place acc any_mark labels index dl_o)