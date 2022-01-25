open AST
open BinNums
open Coqlib
open Datatypes
open Errors
open Integers
open Kildall
open Lattice
open List0
open Maps
open Memdata
open NeedDomain
open NeedOp
open Op
open RTL
open Registers
open ValueAnalysis
open ValueDomain

(** val add_need_all : reg -> nenv -> nenv **)

let add_need_all r ne =
  NE.set r All ne

(** val add_need : reg -> nval -> nenv -> nenv **)

let add_need r nv ne =
  NE.set r (nlub nv (NE.get r ne)) ne

(** val add_needs_all : reg list -> nenv -> nenv **)

let rec add_needs_all rl ne =
  match rl with
  | [] -> ne
  | r1 :: rs -> add_need_all r1 (add_needs_all rs ne)

(** val add_needs : reg list -> nval list -> nenv -> nenv **)

let rec add_needs rl nvl ne =
  match rl with
  | [] -> ne
  | r1 :: rs ->
    (match nvl with
     | [] -> add_needs_all rl ne
     | nv1 :: nvs -> add_need r1 nv1 (add_needs rs nvs ne))

(** val add_ros_need_all : (reg, ident) sum -> nenv -> nenv **)

let add_ros_need_all ros ne =
  match ros with
  | Coq_inl r -> add_need_all r ne
  | Coq_inr _ -> ne

(** val add_opt_need_all : reg option -> nenv -> nenv **)

let add_opt_need_all or0 ne =
  match or0 with
  | Some r -> add_need_all r ne
  | None -> ne

(** val kill : reg -> nenv -> nenv **)

let kill r ne =
  NE.set r Nothing ne

(** val is_dead : nval -> bool **)

let is_dead = function
| Nothing -> true
| _ -> false

(** val is_int_zero : nval -> bool **)

let is_int_zero = function
| NeedDomain.I n -> Int.eq n Int.zero
| _ -> false

(** val transfer_builtin_arg : nval -> NA.t -> reg builtin_arg -> NA.t **)

let rec transfer_builtin_arg nv na a =
  let (ne, nm) = na in
  (match a with
   | BA r -> ((add_need r nv ne), nm)
   | BA_loadstack (chunk, ofs) ->
     (ne, (nmem_add nm (Stk ofs) (size_chunk chunk)))
   | BA_loadglobal (chunk, id, ofs) ->
     (ne, (nmem_add nm (Gl (id, ofs)) (size_chunk chunk)))
   | BA_splitlong (hi, lo) ->
     transfer_builtin_arg All (transfer_builtin_arg All na hi) lo
   | BA_addptr (hi, lo) ->
     transfer_builtin_arg All (transfer_builtin_arg All na hi) lo
   | _ -> (ne, nm))

(** val transfer_builtin_args : NA.t -> reg builtin_arg list -> NA.t **)

let transfer_builtin_args na al =
  fold_left (transfer_builtin_arg All) al na

(** val kill_builtin_res : reg builtin_res -> NE.t -> NE.t **)

let kill_builtin_res res0 ne =
  match res0 with
  | BR r -> kill r ne
  | _ -> ne

(** val transfer_builtin :
    VA.t -> external_function -> reg builtin_arg list -> reg builtin_res ->
    NE.t -> nmem -> NA.t **)

let transfer_builtin app ef args res0 ne nm =
  match ef with
  | EF_vload chunk ->
    (match args with
     | [] -> transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args
     | a1 :: l ->
       (match l with
        | [] ->
          transfer_builtin_arg All ((kill_builtin_res res0 ne),
            (nmem_add nm (aaddr_arg app a1) (size_chunk chunk))) a1
        | _ :: _ ->
          transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args))
  | EF_vstore chunk ->
    (match args with
     | [] -> transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args
     | a1 :: l ->
       (match l with
        | [] ->
          transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args
        | a2 :: l0 ->
          (match l0 with
           | [] ->
             transfer_builtin_arg All
               (transfer_builtin_arg (store_argument chunk)
                 ((kill_builtin_res res0 ne), nm) a2) a1
           | _ :: _ ->
             transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args)))
  | EF_memcpy (sz, _) ->
    (match args with
     | [] -> transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args
     | dst :: l ->
       (match l with
        | [] ->
          transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args
        | src :: l0 ->
          (match l0 with
           | [] ->
             if nmem_contains nm (aaddr_arg app dst) sz
             then transfer_builtin_args ((kill_builtin_res res0 ne),
                    (nmem_add (nmem_remove nm (aaddr_arg app dst) sz)
                      (aaddr_arg app src) sz)) args
             else (ne, nm)
           | _ :: _ ->
             transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args)))
  | EF_annot (_, _, _) ->
    transfer_builtin_args ((kill_builtin_res res0 ne), nm) args
  | EF_annot_val (_, _, _) ->
    transfer_builtin_args ((kill_builtin_res res0 ne), nm) args
  | EF_debug (_, _, _) -> ((kill_builtin_res res0 ne), nm)
  | _ -> transfer_builtin_args ((kill_builtin_res res0 ne), nmem_all) args

(** val transfer : coq_function -> VA.t PMap.t -> node -> NA.t -> NA.t **)

let transfer f approx pc after = match after with
| (ne, nm) ->
  (match PTree.get pc f.fn_code with
   | Some i ->
     (match i with
      | Inop _ -> after
      | Iop (op, args, res0, _) ->
        let nres = nreg ne res0 in
        if is_dead nres
        then after
        else if is_int_zero nres
             then ((kill res0 ne), nm)
             else ((add_needs args (needs_of_operation op nres)
                     (kill res0 ne)), nm)
      | Iload (chunk, addr, args, dst, _) ->
        let ndst = nreg ne dst in
        if is_dead ndst
        then after
        else if is_int_zero ndst
             then ((kill dst ne), nm)
             else ((add_needs_all args (kill dst ne)),
                    (nmem_add nm (aaddressing (PMap.get pc approx) addr args)
                      (size_chunk chunk)))
      | Istore (chunk, addr, args, src, _) ->
        let p = aaddressing (PMap.get pc approx) addr args in
        if nmem_contains nm p (size_chunk chunk)
        then ((add_needs_all args (add_need src (store_argument chunk) ne)),
               (nmem_remove nm p (size_chunk chunk)))
        else after
      | Icall (_, ros, args, res0, _) ->
        ((add_needs_all args (add_ros_need_all ros (kill res0 ne))), nmem_all)
      | Itailcall (_, ros, args) ->
        ((add_needs_all args (add_ros_need_all ros NE.bot)),
          (nmem_dead_stack f.fn_stacksize))
      | Ibuiltin (ef, args, res0, _) ->
        transfer_builtin (PMap.get pc approx) ef args res0 ne nm
      | Icond (cond, args, s1, s2) ->
        if peq s1 s2
        then after
        else ((add_needs args (needs_of_condition cond) ne), nm)
      | Ijumptable (arg, _) -> ((add_need_all arg ne), nm)
      | Ireturn optarg ->
        ((add_opt_need_all optarg ne), (nmem_dead_stack f.fn_stacksize)))
   | None -> NA.bot)

module DS = Backward_Dataflow_Solver(NA)(NodeSetBackward)

(** val analyze : VA.t PMap.t -> coq_function -> NA.t PMap.t option **)

let analyze approx f =
  DS.fixpoint f.fn_code successors_instr (transfer f approx)

(** val transf_instr :
    VA.t PMap.t -> NA.t PMap.t -> node -> instruction -> instruction **)

let transf_instr approx an pc instr = match instr with
| Iop (op, args, res0, s) ->
  let nres = nreg (fst (PMap.get pc an)) res0 in
  if is_dead nres
  then Inop s
  else if is_int_zero nres
       then Iop ((Ointconst Int.zero), [], res0, s)
       else if operation_is_redundant op nres
            then (match args with
                  | [] -> instr
                  | arg :: _ -> Iop (Omove, (arg :: []), res0, s))
            else instr
| Iload (_, _, _, dst, s) ->
  let ndst = nreg (fst (PMap.get pc an)) dst in
  if is_dead ndst
  then Inop s
  else if is_int_zero ndst
       then Iop ((Ointconst Int.zero), [], dst, s)
       else instr
| Istore (chunk, addr, args, _, s) ->
  let p = aaddressing (PMap.get pc approx) addr args in
  if nmem_contains (snd (PMap.get pc an)) p (size_chunk chunk)
  then instr
  else Inop s
| Ibuiltin (e, l, _, s) ->
  (match e with
   | EF_memcpy (sz, _) ->
     (match l with
      | [] -> instr
      | dst :: l0 ->
        (match l0 with
         | [] -> instr
         | _ :: l1 ->
           (match l1 with
            | [] ->
              if nmem_contains (snd (PMap.get pc an))
                   (aaddr_arg (PMap.get pc approx) dst) sz
              then instr
              else Inop s
            | _ :: _ -> instr)))
   | _ -> instr)
| Icond (_, _, s1, s2) -> if peq s1 s2 then Inop s1 else instr
| _ -> instr

(** val transf_function : romem -> coq_function -> coq_function res **)

let transf_function rm f =
  let approx = ValueAnalysis.analyze rm f in
  (match analyze approx f with
   | Some an ->
     OK { fn_sig = f.fn_sig; fn_params = f.fn_params; fn_stacksize =
       f.fn_stacksize; fn_code =
       (PTree.map (transf_instr approx an) f.fn_code); fn_entrypoint =
       f.fn_entrypoint }
   | None ->
     Error
       (msg
         ('N'::('e'::('e'::('d'::('e'::('d'::('n'::('e'::('s'::('s'::(' '::('a'::('n'::('a'::('l'::('y'::('s'::('i'::('s'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))))))))

(** val transf_fundef : romem -> fundef -> fundef res **)

let transf_fundef rm fd =
  transf_partial_fundef (transf_function rm) fd

(** val transf_program : program -> program res **)

let transf_program p =
  transform_partial_program (transf_fundef (romem_for p)) p
