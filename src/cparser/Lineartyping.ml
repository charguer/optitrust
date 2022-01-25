open AST
open BinNums
open Conventions
open Coqlib
open Linear
open List0
open Locations
open Machregs
open Op
open Znumtheory

(** val slot_valid : coq_function -> slot -> coq_Z -> typ -> bool **)

let slot_valid funct sl ofs ty =
  (&&)
    (match sl with
     | Incoming ->
       (fun x -> x)
         (in_dec Loc.eq (S (Incoming, ofs, ty))
           (regs_of_rpairs (loc_parameters funct.fn_sig)))
     | _ -> (fun x -> x) (zle Z0 ofs))
    ((fun x -> x) (coq_Zdivide_dec (typealign ty) ofs))

(** val slot_writable : slot -> bool **)

let slot_writable = function
| Incoming -> false
| _ -> true

(** val loc_valid : coq_function -> loc -> bool **)

let loc_valid funct = function
| R _ -> true
| S (sl, ofs, ty) ->
  (match sl with
   | Local -> slot_valid funct Local ofs ty
   | _ -> false)

(** val wt_builtin_res : typ -> mreg builtin_res -> bool **)

let rec wt_builtin_res ty = function
| BR r -> subtype ty (mreg_type r)
| BR_none -> true
| BR_splitlong (hi, lo) ->
  (&&) (wt_builtin_res Tint hi) (wt_builtin_res Tint lo)

(** val wt_instr : coq_function -> instruction -> bool **)

let wt_instr funct = function
| Lgetstack (sl, ofs, ty, r) ->
  (&&) (subtype ty (mreg_type r)) (slot_valid funct sl ofs ty)
| Lsetstack (_, sl, ofs, ty) ->
  (&&) (slot_valid funct sl ofs ty) (slot_writable sl)
| Lop (op, args, res) ->
  (match is_move_operation op args with
   | Some arg -> subtype (mreg_type arg) (mreg_type res)
   | None ->
     let (_, tres) = type_of_operation op in subtype tres (mreg_type res))
| Lload (chunk, _, _, dst) -> subtype (type_of_chunk chunk) (mreg_type dst)
| Ltailcall (sg, _) -> (fun x -> x) (zeq (size_arguments sg) Z0)
| Lbuiltin (ef, args, res) ->
  (&&) (wt_builtin_res (proj_sig_res (ef_sig ef)) res)
    (forallb (loc_valid funct) (params_of_builtin_args args))
| _ -> true

(** val wt_code : coq_function -> code -> bool **)

let wt_code f c =
  forallb (wt_instr f) c

(** val wt_function : coq_function -> bool **)

let wt_function f =
  wt_code f f.fn_code
