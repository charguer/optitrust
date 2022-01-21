open AST
open Builtins
open Compopts
open ConstpropOp
open Coqlib
open Datatypes
open Integers
open List0
open Machregs
open Maps
open Op
open RTL
open Registers
open ValueAOp
open ValueAnalysis
open ValueDomain

(** val transf_ros : AE.t -> (reg, ident) sum -> (reg, ident) sum **)

let transf_ros ae ros = match ros with
| Coq_inl r ->
  (match areg ae r with
   | Ptr p ->
     (match p with
      | Gl (symb, ofs) ->
        if Ptrofs.eq ofs Ptrofs.zero then Coq_inr symb else ros
      | _ -> ros)
   | _ -> ros)
| Coq_inr _ -> ros

(** val successor_rec : nat -> coq_function -> AE.t -> node -> node **)

let rec successor_rec n f ae pc =
  match n with
  | O -> pc
  | S n' ->
    (match PTree.get pc f.fn_code with
     | Some i ->
       (match i with
        | Inop s -> successor_rec n' f ae s
        | Icond (cond, args, s1, s2) ->
          (match resolve_branch (eval_static_condition cond (aregs ae args)) with
           | Some b -> successor_rec n' f ae (if b then s1 else s2)
           | None -> pc)
        | _ -> pc)
     | None -> pc)

(** val num_iter : nat **)

let num_iter =
  S (S (S (S (S (S (S (S (S (S O)))))))))

(** val successor : coq_function -> AE.t -> node -> node **)

let successor f ae pc =
  successor_rec num_iter f ae pc

(** val builtin_arg_reduction : AE.t -> reg builtin_arg -> reg builtin_arg **)

let rec builtin_arg_reduction ae a = match a with
| BA r ->
  (match areg ae r with
   | I n -> BA_int n
   | L n -> BA_long n
   | F n -> if generate_float_constants () then BA_float n else a
   | FS n -> if generate_float_constants () then BA_single n else a
   | _ -> a)
| BA_splitlong (hi, lo) ->
  (match builtin_arg_reduction ae hi with
   | BA_int nhi ->
     let hi' = BA_int nhi in
     (match builtin_arg_reduction ae lo with
      | BA_int nlo -> BA_long (Int64.ofwords nhi nlo)
      | x -> BA_splitlong (hi', x))
   | x -> BA_splitlong (x, (builtin_arg_reduction ae lo)))
| BA_addptr (a1, a2) ->
  BA_addptr ((builtin_arg_reduction ae a1), (builtin_arg_reduction ae a2))
| _ -> a

(** val builtin_arg_strength_reduction :
    AE.t -> reg builtin_arg -> builtin_arg_constraint -> reg builtin_arg **)

let builtin_arg_strength_reduction ae a c =
  let a' = builtin_arg_reduction ae a in if builtin_arg_ok a' c then a' else a

(** val builtin_args_strength_reduction :
    AE.t -> reg builtin_arg list -> builtin_arg_constraint list -> reg
    builtin_arg list **)

let rec builtin_args_strength_reduction ae al cl =
  match al with
  | [] -> []
  | a :: al0 ->
    (builtin_arg_strength_reduction ae a (hd OK_default cl)) :: (builtin_args_strength_reduction
                                                                  ae al0
                                                                  (tl cl))

(** val debug_strength_reduction :
    AE.t -> reg builtin_arg list -> reg builtin_arg list **)

let rec debug_strength_reduction ae = function
| [] -> []
| a :: al0 ->
  let a' = builtin_arg_reduction ae a in
  let al' = a :: (debug_strength_reduction ae al0) in
  (match a with
   | BA _ ->
     (match a' with
      | BA_int _ -> a' :: al'
      | BA_long _ -> a' :: al'
      | BA_float _ -> a' :: al'
      | BA_single _ -> a' :: al'
      | _ -> al')
   | _ -> al')

(** val builtin_strength_reduction :
    AE.t -> external_function -> reg builtin_arg list -> reg builtin_arg list **)

let builtin_strength_reduction ae ef al =
  match ef with
  | EF_debug (_, _, _) -> debug_strength_reduction ae al
  | _ -> builtin_args_strength_reduction ae al (builtin_constraints ef)

(** val transf_instr :
    coq_function -> VA.t PMap.t -> romem -> node -> instruction -> instruction **)

let transf_instr f an rm pc instr =
  match PMap.get pc an with
  | VA.Bot -> instr
  | VA.State (ae, am) ->
    (match instr with
     | Iop (op, args, res, s) ->
       let aargs = aregs ae args in
       let a = eval_static_operation op aargs in
       let s' = successor f (AE.set res a ae) s in
       (match const_for_result a with
        | Some cop -> Iop (cop, [], res, s')
        | None ->
          let (op', args') = op_strength_reduction op args aargs in
          Iop (op', args', res, s'))
     | Iload (chunk, addr, args, dst, s) ->
       let aargs = aregs ae args in
       let a = loadv chunk rm am (eval_static_addressing addr aargs) in
       (match const_for_result a with
        | Some cop -> Iop (cop, [], dst, s)
        | None ->
          let (addr', args') = addr_strength_reduction addr args aargs in
          Iload (chunk, addr', args', dst, s))
     | Istore (chunk, addr, args, src, s) ->
       let aargs = aregs ae args in
       let (addr', args') = addr_strength_reduction addr args aargs in
       Istore (chunk, addr', args', src, s)
     | Icall (sig0, ros, args, res, s) ->
       Icall (sig0, (transf_ros ae ros), args, res, s)
     | Itailcall (sig0, ros, args) ->
       Itailcall (sig0, (transf_ros ae ros), args)
     | Ibuiltin (ef, args, res, s) ->
       let dfl = Ibuiltin (ef, (builtin_strength_reduction ae ef args), res,
         s)
       in
       (match ef with
        | EF_builtin (name, sg) ->
          (match res with
           | BR rd ->
             (match lookup_builtin_function name sg with
              | Some bf ->
                (match eval_static_builtin_function ae am rm bf args with
                 | Some a ->
                   (match const_for_result a with
                    | Some cop -> Iop (cop, [], rd, s)
                    | None -> dfl)
                 | None -> dfl)
              | None -> dfl)
           | _ -> dfl)
        | _ -> dfl)
     | Icond (cond, args, s1, s2) ->
       let aargs = aregs ae args in
       (match resolve_branch (eval_static_condition cond aargs) with
        | Some b -> if b then Inop s1 else Inop s2
        | None ->
          let (cond', args') = cond_strength_reduction cond args aargs in
          Icond (cond', args', s1, s2))
     | Ijumptable (arg, tbl) ->
       (match areg ae arg with
        | I n ->
          (match list_nth_z tbl (Int.unsigned n) with
           | Some s -> Inop s
           | None -> instr)
        | _ -> instr)
     | _ -> instr)

(** val transf_function : romem -> coq_function -> coq_function **)

let transf_function rm f =
  let an = analyze rm f in
  { fn_sig = f.fn_sig; fn_params = f.fn_params; fn_stacksize =
  f.fn_stacksize; fn_code = (PTree.map (transf_instr f an rm) f.fn_code);
  fn_entrypoint = f.fn_entrypoint }

(** val transf_fundef : romem -> fundef -> fundef **)

let transf_fundef rm fd =
  transf_fundef (transf_function rm) fd

(** val transf_program : program -> program **)

let transf_program p =
  let rm = romem_for p in transform_program (transf_fundef rm) p
