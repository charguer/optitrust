open AST
open BinInt
open BinNums
open BinPos
open Coqlib
open Datatypes
open Errors
open Integers
open List0
open Maps
open Op
open RTL
open Registers

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type funenv = coq_function PTree.t

(** val add_globdef :
    Inliningaux.inlining_info -> funenv -> (ident * (fundef, unit) globdef)
    -> funenv **)

let add_globdef io fenv = function
| (id, g) ->
  (match g with
   | Gfun f0 ->
     (match f0 with
      | Internal f ->
        if Inliningaux.should_inline io id f
        then PTree.set id f fenv
        else PTree.remove id fenv
      | External _ -> PTree.remove id fenv)
   | Gvar _ -> PTree.remove id fenv)

(** val funenv_program : program -> funenv **)

let funenv_program p =
  let io = Inliningaux.inlining_analysis p in
  fold_left (add_globdef io) p.prog_defs PTree.empty

type state = { st_nextreg : positive; st_nextnode : positive; st_code : 
               code; st_stksize : coq_Z }

type 'a res =
| R of 'a * state

type 'a mon = state -> 'a res

(** val initstate : state **)

let initstate =
  { st_nextreg = Coq_xH; st_nextnode = Coq_xH; st_code = PTree.empty;
    st_stksize = Z0 }

(** val set_instr : node -> instruction -> unit mon **)

let set_instr pc i s =
  R ((), { st_nextreg = s.st_nextreg; st_nextnode = s.st_nextnode; st_code =
    (PTree.set pc i s.st_code); st_stksize = s.st_stksize })

(** val add_instr : instruction -> node mon **)

let add_instr i s =
  let pc = s.st_nextnode in
  R (pc, { st_nextreg = s.st_nextreg; st_nextnode = (Pos.succ pc); st_code =
  (PTree.set pc i s.st_code); st_stksize = s.st_stksize })

(** val reserve_nodes : positive -> positive mon **)

let reserve_nodes numnodes s =
  R (s.st_nextnode, { st_nextreg = s.st_nextreg; st_nextnode =
    (Pos.add s.st_nextnode numnodes); st_code = s.st_code; st_stksize =
    s.st_stksize })

(** val reserve_regs : positive -> positive mon **)

let reserve_regs numregs s =
  R (s.st_nextreg, { st_nextreg = (Pos.add s.st_nextreg numregs);
    st_nextnode = s.st_nextnode; st_code = s.st_code; st_stksize =
    s.st_stksize })

(** val request_stack : coq_Z -> unit mon **)

let request_stack sz s =
  R ((), { st_nextreg = s.st_nextreg; st_nextnode = s.st_nextnode; st_code =
    s.st_code; st_stksize = (Z.max s.st_stksize sz) })

(** val ptree_mfold :
    (positive -> 'a1 -> unit mon) -> 'a1 PTree.t -> unit mon **)

let ptree_mfold f t0 s =
  R ((), (PTree.fold (fun s1 k v -> let R (_, s2) = f k v s1 in s2) t0 s))

type context = { dpc : positive; dreg : positive; dstk : coq_Z;
                 mreg : positive; mstk : coq_Z; retinfo : (node * reg) option }

(** val shiftpos : positive -> positive -> positive **)

let shiftpos p amount =
  Pos.pred (Pos.add p amount)

(** val spc : context -> node -> positive **)

let spc ctx pc =
  shiftpos pc ctx.dpc

(** val sreg : context -> reg -> positive **)

let sreg ctx r =
  shiftpos r ctx.dreg

(** val sregs : context -> reg list -> positive list **)

let sregs ctx rl =
  map (sreg ctx) rl

(** val sros : context -> (reg, ident) sum -> (positive, ident) sum **)

let sros ctx ros =
  sum_left_map (sreg ctx) ros

(** val sop : context -> operation -> operation **)

let sop ctx op =
  shift_stack_operation ctx.dstk op

(** val saddr : context -> addressing -> addressing **)

let saddr ctx addr =
  shift_stack_addressing ctx.dstk addr

(** val sbuiltinarg : context -> reg builtin_arg -> reg builtin_arg **)

let rec sbuiltinarg ctx a = match a with
| BA x -> BA (sreg ctx x)
| BA_loadstack (chunk, ofs) ->
  BA_loadstack (chunk, (Ptrofs.add ofs (Ptrofs.repr ctx.dstk)))
| BA_addrstack ofs -> BA_addrstack (Ptrofs.add ofs (Ptrofs.repr ctx.dstk))
| BA_splitlong (hi, lo) ->
  BA_splitlong ((sbuiltinarg ctx hi), (sbuiltinarg ctx lo))
| BA_addptr (a1, a2) -> BA_addptr ((sbuiltinarg ctx a1), (sbuiltinarg ctx a2))
| _ -> a

(** val sbuiltinres : context -> reg builtin_res -> reg builtin_res **)

let sbuiltinres ctx = function
| BR x -> BR (sreg ctx x)
| _ -> BR_none

(** val initcontext : positive -> positive -> positive -> coq_Z -> context **)

let initcontext dpc0 dreg0 nreg sz =
  { dpc = dpc0; dreg = dreg0; dstk = Z0; mreg = nreg; mstk = (Z.max sz Z0);
    retinfo = None }

(** val min_alignment : coq_Z -> coq_Z **)

let min_alignment sz =
  if zle sz (Zpos Coq_xH)
  then Zpos Coq_xH
  else if zle sz (Zpos (Coq_xO Coq_xH))
       then Zpos (Coq_xO Coq_xH)
       else if zle sz (Zpos (Coq_xO (Coq_xO Coq_xH)))
            then Zpos (Coq_xO (Coq_xO Coq_xH))
            else Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val callcontext :
    context -> positive -> positive -> positive -> coq_Z -> node -> reg ->
    context **)

let callcontext ctx dpc0 dreg0 nreg sz retpc retreg =
  { dpc = dpc0; dreg = dreg0; dstk =
    (align (Z.add ctx.dstk ctx.mstk) (min_alignment sz)); mreg = nreg; mstk =
    (Z.max sz Z0); retinfo = (Some ((spc ctx retpc), (sreg ctx retreg))) }

(** val tailcontext :
    context -> positive -> positive -> positive -> coq_Z -> context **)

let tailcontext ctx dpc0 dreg0 nreg sz =
  { dpc = dpc0; dreg = dreg0; dstk = (align ctx.dstk (min_alignment sz));
    mreg = nreg; mstk = (Z.max sz Z0); retinfo = ctx.retinfo }

(** val add_moves : reg list -> reg list -> node -> node mon **)

let rec add_moves srcs dsts succ0 s =
  match srcs with
  | [] -> R (succ0, s)
  | s1 :: sl ->
    (match dsts with
     | [] -> R (succ0, s)
     | d1 :: dl ->
       let R (vx, s2) = add_instr (Iop (Omove, (s1 :: []), d1, succ0)) s in
       add_moves sl dl vx s2)

type inline_decision =
| Cannot_inline
| Can_inline of ident * coq_function

(** val can_inline : funenv -> (reg, ident) sum -> inline_decision **)

let can_inline fenv = function
| Coq_inl _ -> Cannot_inline
| Coq_inr id ->
  let filtered_var = PTree.get id fenv in
  (match filtered_var with
   | Some f -> Can_inline (id, f)
   | None -> Cannot_inline)

(** val inline_function :
    funenv -> (funenv -> __ -> context -> coq_function -> unit mon) ->
    context -> ident -> coq_function -> reg list -> node -> reg -> node mon **)

let inline_function fenv rec0 ctx id f args retpc retreg =
  let npc = max_pc_function f in
  let nreg = max_reg_function f in
  (fun s1 ->
  let R (vx, s2) = reserve_nodes npc s1 in
  let R (vx0, s3) = reserve_regs nreg s2 in
  let ctx' = callcontext ctx vx vx0 nreg f.fn_stacksize retpc retreg in
  let R (_, s4) = rec0 (PTree.remove id fenv) __ ctx' f s3 in
  add_moves (sregs ctx args) (sregs ctx' f.fn_params)
    (spc ctx' f.fn_entrypoint) s4)

(** val inline_tail_function :
    funenv -> (funenv -> __ -> context -> coq_function -> unit mon) ->
    context -> ident -> coq_function -> reg list -> node mon **)

let inline_tail_function fenv rec0 ctx id f args =
  let npc = max_pc_function f in
  let nreg = max_reg_function f in
  (fun s1 ->
  let R (vx, s2) = reserve_nodes npc s1 in
  let R (vx0, s3) = reserve_regs nreg s2 in
  let ctx' = tailcontext ctx vx vx0 nreg f.fn_stacksize in
  let R (_, s4) = rec0 (PTree.remove id fenv) __ ctx' f s3 in
  add_moves (sregs ctx args) (sregs ctx' f.fn_params)
    (spc ctx' f.fn_entrypoint) s4)

(** val inline_return :
    context -> reg option -> (node * reg) -> instruction **)

let inline_return ctx or0 = function
| (retpc, retreg) ->
  (match or0 with
   | Some r -> Iop (Omove, ((sreg ctx r) :: []), retreg, retpc)
   | None -> Inop retpc)

(** val expand_instr :
    funenv -> (funenv -> __ -> context -> coq_function -> unit mon) ->
    context -> node -> instruction -> unit mon **)

let expand_instr fenv rec0 ctx pc = function
| Inop s -> set_instr (spc ctx pc) (Inop (spc ctx s))
| Iop (op, args, res0, s) ->
  set_instr (spc ctx pc) (Iop ((sop ctx op), (sregs ctx args),
    (sreg ctx res0), (spc ctx s)))
| Iload (chunk, addr, args, dst, s) ->
  set_instr (spc ctx pc) (Iload (chunk, (saddr ctx addr), (sregs ctx args),
    (sreg ctx dst), (spc ctx s)))
| Istore (chunk, addr, args, src, s) ->
  set_instr (spc ctx pc) (Istore (chunk, (saddr ctx addr), (sregs ctx args),
    (sreg ctx src), (spc ctx s)))
| Icall (sg, ros, args, res0, s) ->
  (match can_inline fenv ros with
   | Cannot_inline ->
     set_instr (spc ctx pc) (Icall (sg, (sros ctx ros), (sregs ctx args),
       (sreg ctx res0), (spc ctx s)))
   | Can_inline (id, f) ->
     (fun s1 ->
       let R (vx, s2) = inline_function fenv rec0 ctx id f args s res0 s1 in
       set_instr (spc ctx pc) (Inop vx) s2))
| Itailcall (sg, ros, args) ->
  (match can_inline fenv ros with
   | Cannot_inline ->
     (match ctx.retinfo with
      | Some p ->
        let (rpc, rreg) = p in
        set_instr (spc ctx pc) (Icall (sg, (sros ctx ros), (sregs ctx args),
          rreg, rpc))
      | None ->
        set_instr (spc ctx pc) (Itailcall (sg, (sros ctx ros),
          (sregs ctx args))))
   | Can_inline (id, f) ->
     (fun s1 ->
       let R (vx, s2) = inline_tail_function fenv rec0 ctx id f args s1 in
       set_instr (spc ctx pc) (Inop vx) s2))
| Ibuiltin (ef, args, res0, s) ->
  set_instr (spc ctx pc) (Ibuiltin (ef, (map (sbuiltinarg ctx) args),
    (sbuiltinres ctx res0), (spc ctx s)))
| Icond (cond, args, s1, s2) ->
  set_instr (spc ctx pc) (Icond (cond, (sregs ctx args), (spc ctx s1),
    (spc ctx s2)))
| Ijumptable (r, tbl) ->
  set_instr (spc ctx pc) (Ijumptable ((sreg ctx r), (map (spc ctx) tbl)))
| Ireturn or0 ->
  (match ctx.retinfo with
   | Some rinfo -> set_instr (spc ctx pc) (inline_return ctx or0 rinfo)
   | None -> set_instr (spc ctx pc) (Ireturn (option_map (sreg ctx) or0)))

(** val expand_cfg_rec :
    funenv -> (funenv -> __ -> context -> coq_function -> unit mon) ->
    context -> coq_function -> unit mon **)

let expand_cfg_rec fenv rec0 ctx f s1 =
  let R (_, s2) = request_stack (Z.add ctx.dstk ctx.mstk) s1 in
  ptree_mfold (expand_instr fenv rec0 ctx) f.fn_code s2

(** val expand_cfg : funenv -> context -> coq_function -> unit mon **)

let rec expand_cfg x =
  expand_cfg_rec x (fun y _ -> expand_cfg y)

(** val expand_function : funenv -> coq_function -> context mon **)

let expand_function fenv f =
  let npc = max_pc_function f in
  let nreg = max_reg_function f in
  (fun s1 ->
  let R (vx, s2) = reserve_nodes npc s1 in
  let R (vx0, s3) = reserve_regs nreg s2 in
  let ctx = initcontext vx vx0 nreg f.fn_stacksize in
  let R (_, s4) = expand_cfg fenv ctx f s3 in R (ctx, s4))

(** val transf_function :
    funenv -> coq_function -> coq_function Errors.res **)

let transf_function fenv f =
  let R (ctx, s) = expand_function fenv f initstate in
  if zlt s.st_stksize Ptrofs.max_unsigned
  then OK { fn_sig = f.fn_sig; fn_params = (sregs ctx f.fn_params);
         fn_stacksize = s.st_stksize; fn_code = s.st_code; fn_entrypoint =
         (spc ctx f.fn_entrypoint) }
  else Error
         (msg
           ('I'::('n'::('l'::('i'::('n'::('i'::('n'::('g'::(':'::(' '::('s'::('t'::('a'::('c'::('k'::(' '::('t'::('o'::('o'::(' '::('b'::('i'::('g'::[]))))))))))))))))))))))))

(** val transf_fundef : funenv -> fundef -> fundef Errors.res **)

let transf_fundef fenv fd =
  transf_partial_fundef (transf_function fenv) fd

(** val transf_program : program -> program Errors.res **)

let transf_program p =
  let fenv = funenv_program p in
  transform_partial_program (transf_fundef fenv) p
