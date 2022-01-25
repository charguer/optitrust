open AST
open BinInt
open BinNums
open BinPos
open Builtins
open CSEdomain
open CombineOp
open Coqlib
open Datatypes
open Errors
open Integers
open Kildall
open List0
open Maps
open Memdata
open Op
open RTL
open Registers
open ValueAnalysis
open ValueDomain

(** val valnum_reg : numbering -> reg -> numbering * valnum **)

let valnum_reg n r =
  match PTree.get r n.num_reg with
  | Some v -> (n, v)
  | None ->
    let v = n.num_next in
    ({ num_next = (Pos.succ v); num_eqs = n.num_eqs; num_reg =
    (PTree.set r v n.num_reg); num_val = (PMap.set v (r :: []) n.num_val) },
    v)

(** val valnum_regs : numbering -> reg list -> numbering * valnum list **)

let rec valnum_regs n = function
| [] -> (n, [])
| r1 :: rs ->
  let (n1, v1) = valnum_reg n r1 in
  let (ns, vs) = valnum_regs n1 rs in (ns, (v1 :: vs))

(** val find_valnum_rhs : rhs -> equation list -> valnum option **)

let rec find_valnum_rhs r = function
| [] -> None
| e :: eqs1 ->
  let CSEdomain.Eq (v, str, r') = e in
  if (&&) str ((fun x -> x) (eq_rhs r r'))
  then Some v
  else find_valnum_rhs r eqs1

(** val find_valnum_rhs' : rhs -> equation list -> valnum option **)

let rec find_valnum_rhs' r = function
| [] -> None
| e :: eqs1 ->
  let CSEdomain.Eq (v, _, r') = e in
  if eq_rhs r r' then Some v else find_valnum_rhs' r eqs1

(** val find_valnum_num : valnum -> equation list -> rhs option **)

let rec find_valnum_num v = function
| [] -> None
| e :: eqs1 ->
  let CSEdomain.Eq (v', str, r') = e in
  if (&&) str ((fun x -> x) (peq v v'))
  then Some r'
  else find_valnum_num v eqs1

(** val reg_valnum : numbering -> valnum -> reg option **)

let reg_valnum n vn =
  match PMap.get vn n.num_val with
  | [] -> None
  | r :: _ -> Some r

(** val regs_valnums : numbering -> valnum list -> reg list option **)

let rec regs_valnums n = function
| [] -> Some []
| v1 :: vs ->
  (match reg_valnum n v1 with
   | Some r1 ->
     (match regs_valnums n vs with
      | Some rs -> Some (r1 :: rs)
      | None -> None)
   | None -> None)

(** val find_rhs : numbering -> rhs -> reg option **)

let find_rhs n rh =
  match find_valnum_rhs' rh n.num_eqs with
  | Some vres -> reg_valnum n vres
  | None -> None

(** val forget_reg : numbering -> reg -> reg list PMap.t **)

let forget_reg n rd =
  match PTree.get rd n.num_reg with
  | Some v -> PMap.set v (remove peq rd (PMap.get v n.num_val)) n.num_val
  | None -> n.num_val

(** val update_reg : numbering -> reg -> valnum -> reg list PMap.t **)

let update_reg n rd vn =
  let nv = forget_reg n rd in PMap.set vn (rd :: (PMap.get vn nv)) nv

(** val add_rhs : numbering -> reg -> rhs -> numbering **)

let add_rhs n rd rh =
  match find_valnum_rhs rh n.num_eqs with
  | Some vres ->
    { num_next = n.num_next; num_eqs = n.num_eqs; num_reg =
      (PTree.set rd vres n.num_reg); num_val = (update_reg n rd vres) }
  | None ->
    { num_next = (Pos.succ n.num_next); num_eqs = ((CSEdomain.Eq (n.num_next,
      true, rh)) :: n.num_eqs); num_reg =
      (PTree.set rd n.num_next n.num_reg); num_val =
      (update_reg n rd n.num_next) }

(** val add_op : numbering -> reg -> operation -> reg list -> numbering **)

let add_op n rd op rs =
  match is_move_operation op rs with
  | Some r ->
    let (n1, v) = valnum_reg n r in
    { num_next = n1.num_next; num_eqs = n1.num_eqs; num_reg =
    (PTree.set rd v n1.num_reg); num_val = (update_reg n1 rd v) }
  | None -> let (n1, vs) = valnum_regs n rs in add_rhs n1 rd (Op (op, vs))

(** val add_load :
    numbering -> reg -> memory_chunk -> addressing -> reg list -> numbering **)

let add_load n rd chunk addr rs =
  let (n1, vs) = valnum_regs n rs in add_rhs n1 rd (Load (chunk, addr, vs))

(** val set_unknown : numbering -> reg -> numbering **)

let set_unknown n rd =
  { num_next = n.num_next; num_eqs = n.num_eqs; num_reg =
    (PTree.remove rd n.num_reg); num_val = (forget_reg n rd) }

(** val set_res_unknown : numbering -> reg builtin_res -> numbering **)

let set_res_unknown n = function
| BR r -> set_unknown n r
| _ -> n

(** val kill_eqs : (rhs -> bool) -> equation list -> equation list **)

let rec kill_eqs pred = function
| [] -> []
| eq :: rem ->
  let CSEdomain.Eq (_, _, r) = eq in
  if pred r then kill_eqs pred rem else eq :: (kill_eqs pred rem)

(** val kill_equations : (rhs -> bool) -> numbering -> numbering **)

let kill_equations pred n =
  { num_next = n.num_next; num_eqs = (kill_eqs pred n.num_eqs); num_reg =
    n.num_reg; num_val = n.num_val }

(** val filter_loads : rhs -> bool **)

let filter_loads = function
| Op (op, _) -> op_depends_on_memory op
| Load (_, _, _) -> true

(** val kill_all_loads : numbering -> numbering **)

let kill_all_loads n =
  kill_equations filter_loads n

(** val filter_after_store :
    VA.t -> numbering -> aptr -> coq_Z -> rhs -> bool **)

let filter_after_store app n p sz = function
| Op (op, _) -> op_depends_on_memory op
| Load (chunk, addr, vl) ->
  (match regs_valnums n vl with
   | Some rl ->
     negb (pdisjoint (aaddressing app addr rl) (size_chunk chunk) p sz)
   | None -> true)

(** val kill_loads_after_store :
    VA.t -> numbering -> memory_chunk -> addressing -> reg list -> numbering **)

let kill_loads_after_store app n chunk addr args =
  let p = aaddressing app addr args in
  kill_equations (filter_after_store app n p (size_chunk chunk)) n

(** val store_normalized_range : memory_chunk -> aval **)

let store_normalized_range = function
| Mint8signed -> Sgn (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| Mint8unsigned -> Uns (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
| Mint16signed ->
  Sgn (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| Mint16unsigned ->
  Uns (Ptop, (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
| _ -> coq_Vtop

(** val add_store_result :
    VA.t -> numbering -> memory_chunk -> addressing -> reg list -> reg ->
    numbering **)

let add_store_result app n chunk addr rargs rsrc =
  if vincl (avalue app rsrc) (store_normalized_range chunk)
  then let (n1, vsrc) = valnum_reg n rsrc in
       let (n2, vargs) = valnum_regs n1 rargs in
       { num_next = n2.num_next; num_eqs = ((CSEdomain.Eq (vsrc, false, (Load
       (chunk, addr, vargs)))) :: n2.num_eqs); num_reg = n2.num_reg;
       num_val = n2.num_val }
  else n

(** val kill_loads_after_storebytes :
    VA.t -> numbering -> aptr -> coq_Z -> numbering **)

let kill_loads_after_storebytes app n dst sz =
  kill_equations (filter_after_store app n dst sz) n

(** val shift_memcpy_eq :
    coq_Z -> coq_Z -> coq_Z -> equation -> equation option **)

let shift_memcpy_eq src sz delta = function
| CSEdomain.Eq (l, strict, r) ->
  (match r with
   | Op (_, _) -> None
   | Load (chunk, a, _) ->
     (match a with
      | Ainstack i ->
        let i0 = Ptrofs.unsigned i in
        let j = Z.add i0 delta in
        if (&&)
             ((&&)
               ((&&)
                 ((&&) ((fun x -> x) (zle src i0))
                   ((fun x -> x)
                     (zle (Z.add i0 (size_chunk chunk)) (Z.add src sz))))
                 ((fun x -> x) (zeq (Z.modulo delta (align_chunk chunk)) Z0)))
               ((fun x -> x) (zle Z0 j)))
             ((fun x -> x) (zle j Ptrofs.max_unsigned))
        then Some (CSEdomain.Eq (l, strict, (Load (chunk, (Ainstack
               (Ptrofs.repr j)), []))))
        else None
      | _ -> None))

(** val add_memcpy_eqs :
    coq_Z -> coq_Z -> coq_Z -> equation list -> equation list -> equation list **)

let rec add_memcpy_eqs src sz delta eqs1 eqs2 =
  match eqs1 with
  | [] -> eqs2
  | e :: eqs ->
    (match shift_memcpy_eq src sz delta e with
     | Some e' -> e' :: (add_memcpy_eqs src sz delta eqs eqs2)
     | None -> add_memcpy_eqs src sz delta eqs eqs2)

(** val add_memcpy :
    numbering -> numbering -> aptr -> aptr -> coq_Z -> numbering **)

let add_memcpy n1 n2 asrc adst sz =
  match asrc with
  | Stk src ->
    (match adst with
     | Stk dst ->
       { num_next = n2.num_next; num_eqs =
         (add_memcpy_eqs (Ptrofs.unsigned src) sz
           (Z.sub (Ptrofs.unsigned dst) (Ptrofs.unsigned src)) n1.num_eqs
           n2.num_eqs); num_reg = n2.num_reg; num_val = n2.num_val }
     | _ -> n2)
  | _ -> n2

(** val reduce_rec :
    ((valnum -> rhs option) -> 'a1 -> valnum list -> ('a1 * valnum list)
    option) -> numbering -> nat -> 'a1 -> valnum list -> ('a1 * reg list)
    option **)

let rec reduce_rec f n niter op args =
  match niter with
  | O -> None
  | S niter' ->
    (match f (fun v -> find_valnum_num v n.num_eqs) op args with
     | Some p ->
       let (op', args') = p in
       (match reduce_rec f n niter' op' args' with
        | Some p0 -> Some p0
        | None ->
          (match regs_valnums n args' with
           | Some rl -> Some (op', rl)
           | None -> None))
     | None -> None)

(** val reduce :
    ((valnum -> rhs option) -> 'a1 -> valnum list -> ('a1 * valnum list)
    option) -> numbering -> 'a1 -> reg list -> valnum list -> 'a1 * reg list **)

let reduce f n op rl vl =
  match reduce_rec f n (S (S (S (S O)))) op vl with
  | Some res0 -> res0
  | None -> (op, rl)

module Numbering =
 struct
  type t = numbering

  (** val top : numbering **)

  let top =
    empty_numbering
 end

module Solver = BBlock_solver(Numbering)

(** val transfer :
    coq_function -> VA.t PMap.t -> node -> numbering -> numbering **)

let transfer f approx pc before =
  match PTree.get pc f.fn_code with
  | Some i ->
    (match i with
     | Iop (op, args, res0, _) -> add_op before res0 op args
     | Iload (chunk, addr, args, dst, _) ->
       add_load before dst chunk addr args
     | Istore (chunk, addr, args, src, _) ->
       let app = PMap.get pc approx in
       let n = kill_loads_after_store app before chunk addr args in
       add_store_result app n chunk addr args src
     | Icall (_, _, _, _, _) -> empty_numbering
     | Itailcall (_, _, _) -> empty_numbering
     | Ibuiltin (ef, args, res0, _) ->
       (match ef with
        | EF_builtin (name, sg) ->
          (match lookup_builtin_function name sg with
           | Some _ -> set_res_unknown before res0
           | None -> set_res_unknown (kill_all_loads before) res0)
        | EF_vload _ -> set_res_unknown before res0
        | EF_vstore _ -> set_res_unknown (kill_all_loads before) res0
        | EF_memcpy (sz, _) ->
          (match args with
           | [] -> empty_numbering
           | dst :: l ->
             (match l with
              | [] -> empty_numbering
              | src :: l0 ->
                (match l0 with
                 | [] ->
                   let app = PMap.get pc approx in
                   let adst = aaddr_arg app dst in
                   let asrc = aaddr_arg app src in
                   let n = kill_loads_after_storebytes app before adst sz in
                   set_res_unknown (add_memcpy before n asrc adst sz) res0
                 | _ :: _ -> empty_numbering)))
        | EF_annot (_, _, _) -> set_res_unknown before res0
        | EF_annot_val (_, _, _) -> set_res_unknown before res0
        | EF_debug (_, _, _) -> set_res_unknown before res0
        | _ -> empty_numbering)
     | _ -> before)
  | None -> before

(** val analyze : coq_function -> VA.t PMap.t -> numbering PMap.t option **)

let analyze f approx =
  Solver.fixpoint f.fn_code successors_instr (transfer f approx)
    f.fn_entrypoint

(** val transf_instr : numbering -> instruction -> instruction **)

let transf_instr n instr = match instr with
| Iop (op, args, res0, s) ->
  if is_trivial_op op
  then instr
  else let (n1, vl) = valnum_regs n args in
       (match find_rhs n1 (Op (op, vl)) with
        | Some r -> Iop (Omove, (r :: []), res0, s)
        | None ->
          let (op', args') = reduce combine_op n1 op args vl in
          Iop (op', args', res0, s))
| Iload (chunk, addr, args, dst, s) ->
  let (n1, vl) = valnum_regs n args in
  (match find_rhs n1 (Load (chunk, addr, vl)) with
   | Some r -> Iop (Omove, (r :: []), dst, s)
   | None ->
     let (addr', args') = reduce combine_addr n1 addr args vl in
     Iload (chunk, addr', args', dst, s))
| Istore (chunk, addr, args, src, s) ->
  let (n1, vl) = valnum_regs n args in
  let (addr', args') = reduce combine_addr n1 addr args vl in
  Istore (chunk, addr', args', src, s)
| Icond (cond, args, s1, s2) ->
  let (n1, vl) = valnum_regs n args in
  let (cond', args') = reduce combine_cond n1 cond args vl in
  Icond (cond', args', s1, s2)
| _ -> instr

(** val transf_code : numbering PMap.t -> code -> code **)

let transf_code approxs instrs =
  PTree.map (fun pc instr -> transf_instr (PMap.get pc approxs) instr) instrs

(** val vanalyze : romem -> coq_function -> VA.t PMap.t **)

let vanalyze =
  ValueAnalysis.analyze

(** val transf_function : romem -> coq_function -> coq_function res **)

let transf_function rm f =
  let approx = vanalyze rm f in
  (match analyze f approx with
   | Some approxs ->
     OK { fn_sig = f.fn_sig; fn_params = f.fn_params; fn_stacksize =
       f.fn_stacksize; fn_code = (transf_code approxs f.fn_code);
       fn_entrypoint = f.fn_entrypoint }
   | None ->
     Error
       (msg
         ('C'::('S'::('E'::(' '::('f'::('a'::('i'::('l'::('u'::('r'::('e'::[])))))))))))))

(** val transf_fundef : romem -> fundef -> fundef res **)

let transf_fundef rm f =
  transf_partial_fundef (transf_function rm) f

(** val transf_program : program -> program res **)

let transf_program p =
  transform_partial_program (transf_fundef (romem_for p)) p
