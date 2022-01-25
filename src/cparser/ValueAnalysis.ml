open AST
open Archi
open BinInt
open BinNums
open Builtins
open Builtins0
open Compopts
open Datatypes
open Kildall
open List0
open Liveness
open Maps
open Op
open RTL
open Registers
open ValueAOp
open ValueDomain

(** val areg : aenv -> reg -> aval **)

let areg ae r =
  AE.get r ae

(** val aregs : aenv -> reg list -> aval list **)

let aregs ae rl =
  map (areg ae) rl

(** val mafter_public_call : amem **)

let mafter_public_call =
  mtop

(** val mafter_private_call : amem -> amem **)

let mafter_private_call am_before =
  { am_stack = am_before.am_stack; am_glob = PTree.empty; am_nonstack =
    Nonstack; am_top = (plub am_before.am_stack.ab_summary Nonstack) }

(** val analyze_call : amem -> aval list -> aval * amem **)

let analyze_call am aargs =
  if (&&) (pincl am.am_nonstack Nonstack)
       (forallb (fun av -> vpincl av Nonstack) aargs)
  then ((Ifptr Nonstack), (mafter_private_call am))
  else (coq_Vtop, mafter_public_call)

(** val transfer_call : aenv -> amem -> reg list -> reg -> VA.t' **)

let transfer_call ae am args res =
  let (av, am') = analyze_call am (aregs ae args) in
  VA.State ((AE.set res av ae), am')

(** val abuiltin_arg : aenv -> amem -> romem -> reg builtin_arg -> aval **)

let rec abuiltin_arg ae am rm = function
| BA r -> areg ae r
| BA_int n -> I n
| BA_long n -> L n
| BA_float n -> F n
| BA_single n -> FS n
| BA_loadstack (chunk, ofs) -> loadv chunk rm am (Ptr (Stk ofs))
| BA_addrstack ofs -> Ptr (Stk ofs)
| BA_loadglobal (chunk, id, ofs) -> loadv chunk rm am (Ptr (Gl (id, ofs)))
| BA_addrglobal (id, ofs) -> Ptr (Gl (id, ofs))
| BA_splitlong (hi, lo) ->
  longofwords (abuiltin_arg ae am rm hi) (abuiltin_arg ae am rm lo)
| BA_addptr (ba1, ba2) ->
  let v1 = abuiltin_arg ae am rm ba1 in
  let v2 = abuiltin_arg ae am rm ba2 in
  if ptr64 then addl v1 v2 else add v1 v2

(** val set_builtin_res : reg builtin_res -> aval -> aenv -> aenv **)

let set_builtin_res br av ae =
  match br with
  | BR r -> AE.set r av ae
  | _ -> ae

(** val transfer_builtin_default :
    aenv -> amem -> romem -> reg builtin_arg list -> reg builtin_res -> VA.t' **)

let transfer_builtin_default ae am rm args res =
  let (av, am') = analyze_call am (map (abuiltin_arg ae am rm) args) in
  VA.State ((set_builtin_res res av ae), am')

(** val eval_static_builtin_function :
    aenv -> amem -> romem -> builtin_function -> reg builtin_arg list -> aval
    option **)

let eval_static_builtin_function ae am rm bf args =
  match bs_sem (builtin_function_sig bf).sig_res (builtin_function_sem bf)
          (map val_of_aval (map (abuiltin_arg ae am rm) args)) with
  | Some v -> aval_of_val v
  | None -> None

(** val transfer_builtin :
    aenv -> amem -> romem -> external_function -> reg builtin_arg list -> reg
    builtin_res -> VA.t' **)

let transfer_builtin ae am rm ef args res =
  match ef with
  | EF_builtin (name, sg) ->
    (match lookup_builtin_function name sg with
     | Some bf ->
       (match eval_static_builtin_function ae am rm bf args with
        | Some av -> VA.State ((set_builtin_res res av ae), am)
        | None -> transfer_builtin_default ae am rm args res)
     | None -> transfer_builtin_default ae am rm args res)
  | EF_vload chunk ->
    (match args with
     | [] -> transfer_builtin_default ae am rm args res
     | addr :: l ->
       (match l with
        | [] ->
          let aaddr = abuiltin_arg ae am rm addr in
          let a =
            if va_strict ()
            then vlub (loadv chunk rm am aaddr)
                   (vnormalize chunk (Ifptr Glob))
            else vnormalize chunk coq_Vtop
          in
          VA.State ((set_builtin_res res a ae), am)
        | _ :: _ -> transfer_builtin_default ae am rm args res))
  | EF_vstore chunk ->
    (match args with
     | [] -> transfer_builtin_default ae am rm args res
     | addr :: l ->
       (match l with
        | [] -> transfer_builtin_default ae am rm args res
        | v :: l0 ->
          (match l0 with
           | [] ->
             let aaddr = abuiltin_arg ae am rm addr in
             let av = abuiltin_arg ae am rm v in
             let am' = storev chunk am aaddr av in
             VA.State ((set_builtin_res res ntop ae), (mlub am am'))
           | _ :: _ -> transfer_builtin_default ae am rm args res)))
  | EF_memcpy (sz, _) ->
    (match args with
     | [] -> transfer_builtin_default ae am rm args res
     | dst :: l ->
       (match l with
        | [] -> transfer_builtin_default ae am rm args res
        | src :: l0 ->
          (match l0 with
           | [] ->
             let adst = abuiltin_arg ae am rm dst in
             let asrc = abuiltin_arg ae am rm src in
             let p = loadbytes am rm (aptr_of_aval asrc) in
             let am' = storebytes am (aptr_of_aval adst) sz p in
             VA.State ((set_builtin_res res ntop ae), am')
           | _ :: _ -> transfer_builtin_default ae am rm args res)))
  | EF_annot (_, _, _) -> VA.State ((set_builtin_res res ntop ae), am)
  | EF_annot_val (_, _, _) ->
    (match args with
     | [] -> transfer_builtin_default ae am rm args res
     | v :: l ->
       (match l with
        | [] ->
          let av = abuiltin_arg ae am rm v in
          VA.State ((set_builtin_res res av ae), am)
        | _ :: _ -> transfer_builtin_default ae am rm args res))
  | EF_debug (_, _, _) -> VA.State ((set_builtin_res res ntop ae), am)
  | _ -> transfer_builtin_default ae am rm args res

(** val transfer : coq_function -> romem -> node -> aenv -> amem -> VA.t **)

let transfer f rm pc ae am =
  match PTree.get pc f.fn_code with
  | Some i ->
    (match i with
     | Iop (op, args, res, _) ->
       let a = eval_static_operation op (aregs ae args) in
       VA.State ((AE.set res a ae), am)
     | Iload (chunk, addr, args, dst, _) ->
       let a = loadv chunk rm am (eval_static_addressing addr (aregs ae args))
       in
       VA.State ((AE.set dst a ae), am)
     | Istore (chunk, addr, args, src, _) ->
       let am' =
         storev chunk am (eval_static_addressing addr (aregs ae args))
           (areg ae src)
       in
       VA.State (ae, am')
     | Icall (_, _, args, res, _) -> transfer_call ae am args res
     | Itailcall (_, _, _) -> VA.Bot
     | Ibuiltin (ef, args, res, _) -> transfer_builtin ae am rm ef args res
     | Ireturn _ -> VA.Bot
     | _ -> VA.State (ae, am))
  | None -> VA.Bot

(** val transfer' :
    coq_function -> reg list PTree.t -> romem -> node -> VA.t -> VA.t **)

let transfer' f lastuses rm pc = function
| VA.Bot -> VA.Bot
| VA.State (ae, am) ->
  (match transfer f rm pc ae am with
   | VA.Bot -> VA.Bot
   | VA.State (ae', am') ->
     let ae'' =
       match PTree.get pc lastuses with
       | Some regs -> eforget regs ae'
       | None -> ae'
     in
     VA.State (ae'', am'))

module DS = Dataflow_Solver(VA)(NodeSetForward)

(** val mfunction_entry : amem **)

let mfunction_entry =
  { am_stack = (ablock_init Pbot); am_glob = PTree.empty; am_nonstack =
    Nonstack; am_top = Nonstack }

(** val analyze : romem -> coq_function -> VA.t PMap.t **)

let analyze rm f =
  let lu = last_uses f in
  let entry = VA.State ((einit_regs f.fn_params), mfunction_entry) in
  (match DS.fixpoint f.fn_code successors_instr (transfer' f lu rm)
           f.fn_entrypoint entry with
   | Some res -> res
   | None -> PMap.init (VA.State (AE.top, mtop)))

(** val store_init_data : ablock -> coq_Z -> init_data -> ablock **)

let store_init_data ab p = function
| Init_int8 n -> ablock_store Mint8unsigned ab p (I n)
| Init_int16 n -> ablock_store Mint16unsigned ab p (I n)
| Init_int32 n -> ablock_store Mint32 ab p (I n)
| Init_int64 n -> ablock_store Mint64 ab p (L n)
| Init_float32 n ->
  ablock_store Mfloat32 ab p
    (if propagate_float_constants () then FS n else ntop)
| Init_float64 n ->
  ablock_store Mfloat64 ab p
    (if propagate_float_constants () then F n else ntop)
| Init_space _ -> ab
| Init_addrof (symb, ofs) -> ablock_store coq_Mptr ab p (Ptr (Gl (symb, ofs)))

(** val store_init_data_list : ablock -> coq_Z -> init_data list -> ablock **)

let rec store_init_data_list ab p = function
| [] -> ab
| id :: idl' ->
  store_init_data_list (store_init_data ab p id)
    (Z.add p (init_data_size id)) idl'

(** val definitive_initializer : init_data list -> bool **)

let definitive_initializer = function
| [] -> false
| i :: l ->
  (match i with
   | Init_space _ -> (match l with
                      | [] -> false
                      | _ :: _ -> true)
   | _ -> true)

(** val alloc_global : romem -> (ident * (fundef, unit) globdef) -> romem **)

let alloc_global rm = function
| (id, g) ->
  (match g with
   | Gfun _ -> PTree.remove id rm
   | Gvar v ->
     if (&&) ((&&) v.gvar_readonly (negb v.gvar_volatile))
          (definitive_initializer v.gvar_init)
     then PTree.set id
            (store_init_data_list (ablock_init Pbot) Z0 v.gvar_init) rm
     else PTree.remove id rm)

(** val romem_for : program -> romem **)

let romem_for p =
  fold_left alloc_global p.prog_defs PTree.empty

(** val avalue : VA.t -> reg -> aval **)

let avalue a r =
  match a with
  | VA.Bot -> Vbot
  | VA.State (ae, _) -> AE.get r ae

(** val aaddressing : VA.t -> addressing -> reg list -> aptr **)

let aaddressing a addr args =
  match a with
  | VA.Bot -> Pbot
  | VA.State (ae, _) ->
    aptr_of_aval (eval_static_addressing addr (aregs ae args))

(** val aaddr_arg : VA.t -> reg builtin_arg -> aptr **)

let aaddr_arg a ba =
  match a with
  | VA.Bot -> Pbot
  | VA.State (ae, _) ->
    (match ba with
     | BA r -> aptr_of_aval (AE.get r ae)
     | BA_addrstack ofs -> Stk ofs
     | BA_addrglobal (id, ofs) -> Gl (id, ofs)
     | _ -> Ptop)
