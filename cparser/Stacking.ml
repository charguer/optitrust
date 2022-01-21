open AST
open BinInt
open BinNums
open Bounds
open Coqlib
open Datatypes
open Errors
open Integers
open Linear
open Lineartyping
open List0
open Locations
open Mach
open Machregs
open Op
open Stacklayout

(** val offset_local : frame_env -> coq_Z -> coq_Z **)

let offset_local fe x =
  Z.add fe.fe_ofs_local (Z.mul (Zpos (Coq_xO (Coq_xO Coq_xH))) x)

(** val offset_arg : coq_Z -> coq_Z **)

let offset_arg x =
  Z.add fe_ofs_arg (Z.mul (Zpos (Coq_xO (Coq_xO Coq_xH))) x)

(** val save_callee_save_rec : mreg list -> coq_Z -> code -> code **)

let rec save_callee_save_rec rl ofs k =
  match rl with
  | [] -> k
  | r :: rl0 ->
    let ty = mreg_type r in
    let sz = AST.typesize ty in
    let ofs1 = align ofs sz in
    (Msetstack (r, (Ptrofs.repr ofs1),
    ty)) :: (save_callee_save_rec rl0 (Z.add ofs1 sz) k)

(** val save_callee_save : frame_env -> code -> code **)

let save_callee_save fe k =
  save_callee_save_rec fe.fe_used_callee_save fe.fe_ofs_callee_save k

(** val restore_callee_save_rec : mreg list -> coq_Z -> code -> code **)

let rec restore_callee_save_rec rl ofs k =
  match rl with
  | [] -> k
  | r :: rl0 ->
    let ty = mreg_type r in
    let sz = AST.typesize ty in
    let ofs1 = align ofs sz in
    (Mgetstack ((Ptrofs.repr ofs1), ty,
    r)) :: (restore_callee_save_rec rl0 (Z.add ofs1 sz) k)

(** val restore_callee_save : frame_env -> code -> code **)

let restore_callee_save fe k =
  restore_callee_save_rec fe.fe_used_callee_save fe.fe_ofs_callee_save k

(** val transl_op : frame_env -> operation -> operation **)

let transl_op fe op =
  shift_stack_operation fe.fe_stack_data op

(** val transl_addr : frame_env -> addressing -> addressing **)

let transl_addr fe addr =
  shift_stack_addressing fe.fe_stack_data addr

(** val transl_builtin_arg :
    frame_env -> loc builtin_arg -> mreg builtin_arg **)

let rec transl_builtin_arg fe = function
| BA x ->
  (match x with
   | R r -> BA r
   | S (sl, ofs, ty) ->
     (match sl with
      | Local ->
        BA_loadstack ((chunk_of_type ty), (Ptrofs.repr (offset_local fe ofs)))
      | _ -> BA_int Int.zero))
| BA_int n -> BA_int n
| BA_long n -> BA_long n
| BA_float n -> BA_float n
| BA_single n -> BA_single n
| BA_loadstack (chunk, ofs) ->
  BA_loadstack (chunk, (Ptrofs.add ofs (Ptrofs.repr fe.fe_stack_data)))
| BA_addrstack ofs ->
  BA_addrstack (Ptrofs.add ofs (Ptrofs.repr fe.fe_stack_data))
| BA_loadglobal (chunk, id, ofs) -> BA_loadglobal (chunk, id, ofs)
| BA_addrglobal (id, ofs) -> BA_addrglobal (id, ofs)
| BA_splitlong (hi, lo) ->
  BA_splitlong ((transl_builtin_arg fe hi), (transl_builtin_arg fe lo))
| BA_addptr (a1, a2) ->
  BA_addptr ((transl_builtin_arg fe a1), (transl_builtin_arg fe a2))

(** val transl_instr : frame_env -> Linear.instruction -> code -> code **)

let transl_instr fe i k =
  match i with
  | Lgetstack (sl, ofs, ty, r) ->
    (match sl with
     | Local -> (Mgetstack ((Ptrofs.repr (offset_local fe ofs)), ty, r)) :: k
     | Incoming -> (Mgetparam ((Ptrofs.repr (offset_arg ofs)), ty, r)) :: k
     | Outgoing -> (Mgetstack ((Ptrofs.repr (offset_arg ofs)), ty, r)) :: k)
  | Lsetstack (r, sl, ofs, ty) ->
    (match sl with
     | Local -> (Msetstack (r, (Ptrofs.repr (offset_local fe ofs)), ty)) :: k
     | Incoming -> k
     | Outgoing -> (Msetstack (r, (Ptrofs.repr (offset_arg ofs)), ty)) :: k)
  | Lop (op, args, res0) -> (Mop ((transl_op fe op), args, res0)) :: k
  | Lload (chunk, addr, args, dst) ->
    (Mload (chunk, (transl_addr fe addr), args, dst)) :: k
  | Lstore (chunk, addr, args, src) ->
    (Mstore (chunk, (transl_addr fe addr), args, src)) :: k
  | Lcall (sig0, ros) -> (Mcall (sig0, ros)) :: k
  | Ltailcall (sig0, ros) ->
    restore_callee_save fe ((Mtailcall (sig0, ros)) :: k)
  | Lbuiltin (ef, args, dst) ->
    (Mbuiltin (ef, (map (transl_builtin_arg fe) args), dst)) :: k
  | Llabel lbl -> (Mlabel lbl) :: k
  | Lgoto lbl -> (Mgoto lbl) :: k
  | Lcond (cond, args, lbl) -> (Mcond (cond, args, lbl)) :: k
  | Ljumptable (arg, tbl) -> (Mjumptable (arg, tbl)) :: k
  | Lreturn -> restore_callee_save fe (Mreturn :: k)

(** val transl_code : frame_env -> Linear.instruction list -> code **)

let transl_code fe il =
  list_fold_right (transl_instr fe) il []

(** val transl_body : Linear.coq_function -> frame_env -> code **)

let transl_body f fe =
  save_callee_save fe (transl_code fe f.Linear.fn_code)

(** val transf_function : Linear.coq_function -> coq_function res **)

let transf_function f =
  let fe = make_env (function_bounds f) in
  if negb (wt_function f)
  then Error
         (msg
           ('I'::('l'::('l'::('-'::('f'::('o'::('r'::('m'::('e'::('d'::(' '::('L'::('i'::('n'::('e'::('a'::('r'::(' '::('c'::('o'::('d'::('e'::[])))))))))))))))))))))))
  else if zlt Ptrofs.max_unsigned fe.fe_size
       then Error
              (msg
                ('T'::('o'::('o'::(' '::('m'::('a'::('n'::('y'::(' '::('s'::('p'::('i'::('l'::('l'::('e'::('d'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::('s'::(','::(' '::('s'::('t'::('a'::('c'::('k'::(' '::('s'::('i'::('z'::('e'::(' '::('e'::('x'::('c'::('e'::('e'::('d'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))
       else OK { fn_sig = f.Linear.fn_sig; fn_code = (transl_body f fe);
              fn_stacksize = fe.fe_size; fn_link_ofs =
              (Ptrofs.repr fe.fe_ofs_link); fn_retaddr_ofs =
              (Ptrofs.repr fe.fe_ofs_retaddr) }

(** val transf_fundef : Linear.fundef -> fundef res **)

let transf_fundef f =
  transf_partial_fundef transf_function f

(** val transf_program : Linear.program -> program res **)

let transf_program p =
  transform_partial_program transf_fundef p
