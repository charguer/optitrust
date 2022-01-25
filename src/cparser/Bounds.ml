open AST
open BinInt
open BinNums
open Conventions
open Conventions1
open Coqlib
open Datatypes
open FSetAVL
open Int0
open Linear
open List0
open Locations
open Machregs
open Ordered

module RegOrd = OrderedIndexed(IndexedMreg)

module RegSet = Make(RegOrd)

type bounds = { used_callee_save : mreg list; bound_local : coq_Z;
                bound_outgoing : coq_Z; bound_stack_data : coq_Z }

(** val record_reg : RegSet.t -> mreg -> RegSet.t **)

let record_reg u r =
  if is_callee_save r then RegSet.add r u else u

(** val record_regs : RegSet.t -> mreg list -> RegSet.t **)

let record_regs u rl =
  fold_left record_reg rl u

(** val record_regs_of_instr : RegSet.t -> instruction -> RegSet.t **)

let record_regs_of_instr u = function
| Lgetstack (_, _, _, r) -> record_reg u r
| Lsetstack (r, _, _, _) -> record_reg u r
| Lop (_, _, res) -> record_reg u res
| Lload (_, _, _, dst) -> record_reg u dst
| Lbuiltin (ef, _, res) ->
  record_regs (record_regs u (params_of_builtin_res res))
    (destroyed_by_builtin ef)
| _ -> u

(** val record_regs_of_function : coq_function -> RegSet.t **)

let record_regs_of_function f =
  fold_left record_regs_of_instr f.fn_code RegSet.empty

(** val slots_of_locs : loc list -> ((slot * coq_Z) * typ) list **)

let rec slots_of_locs = function
| [] -> []
| l0 :: l' ->
  (match l0 with
   | R _ -> slots_of_locs l'
   | S (sl, ofs, ty) -> ((sl, ofs), ty) :: (slots_of_locs l'))

(** val slots_of_instr : instruction -> ((slot * coq_Z) * typ) list **)

let slots_of_instr = function
| Lgetstack (sl, ofs, ty, _) -> ((sl, ofs), ty) :: []
| Lsetstack (_, sl, ofs, ty) -> ((sl, ofs), ty) :: []
| Lbuiltin (_, args, _) -> slots_of_locs (params_of_builtin_args args)
| _ -> []

(** val max_over_list : ('a1 -> coq_Z) -> 'a1 list -> coq_Z **)

let max_over_list valu l =
  fold_left (fun m l0 -> Z.max m (valu l0)) l Z0

(** val max_over_instrs : coq_function -> (instruction -> coq_Z) -> coq_Z **)

let max_over_instrs f valu =
  max_over_list valu f.fn_code

(** val max_over_slots_of_instr :
    (((slot * coq_Z) * typ) -> coq_Z) -> instruction -> coq_Z **)

let max_over_slots_of_instr valu i =
  max_over_list valu (slots_of_instr i)

(** val max_over_slots_of_funct :
    coq_function -> (((slot * coq_Z) * typ) -> coq_Z) -> coq_Z **)

let max_over_slots_of_funct f valu =
  max_over_instrs f (max_over_slots_of_instr valu)

(** val local_slot : ((slot * coq_Z) * typ) -> coq_Z **)

let local_slot = function
| (p, ty) ->
  let (s0, ofs) = p in
  (match s0 with
   | Local -> Z.add ofs (typesize ty)
   | _ -> Z0)

(** val outgoing_slot : ((slot * coq_Z) * typ) -> coq_Z **)

let outgoing_slot = function
| (p, ty) ->
  let (s0, ofs) = p in
  (match s0 with
   | Outgoing -> Z.add ofs (typesize ty)
   | _ -> Z0)

(** val outgoing_space : instruction -> coq_Z **)

let outgoing_space = function
| Lcall (sig0, _) -> size_arguments sig0
| _ -> Z0

(** val function_bounds : coq_function -> bounds **)

let function_bounds f =
  { used_callee_save = (RegSet.elements (record_regs_of_function f));
    bound_local = (max_over_slots_of_funct f local_slot); bound_outgoing =
    (Z.max (max_over_instrs f outgoing_space)
      (max_over_slots_of_funct f outgoing_slot)); bound_stack_data =
    (Z.max f.fn_stacksize Z0) }

(** val size_callee_save_area_rec : mreg list -> coq_Z -> coq_Z **)

let rec size_callee_save_area_rec l ofs =
  match l with
  | [] -> ofs
  | r :: l0 ->
    let ty = mreg_type r in
    let sz = AST.typesize ty in
    size_callee_save_area_rec l0 (Z.add (align ofs sz) sz)

(** val size_callee_save_area : bounds -> coq_Z -> coq_Z **)

let size_callee_save_area b ofs =
  size_callee_save_area_rec b.used_callee_save ofs

type frame_env = { fe_size : coq_Z; fe_ofs_link : coq_Z;
                   fe_ofs_retaddr : coq_Z; fe_ofs_local : coq_Z;
                   fe_ofs_callee_save : coq_Z; fe_stack_data : coq_Z;
                   fe_used_callee_save : mreg list }
