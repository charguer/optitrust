open AST
open Archi
open BinInt
open BinNums
open Coqlib
open Datatypes
open List0
open Locations
open Machregs

(** val is_callee_save : mreg -> bool **)

let is_callee_save = function
| BX -> true
| SI -> (||) (negb ptr64) win64
| DI -> (||) (negb ptr64) win64
| BP -> true
| R12 -> true
| R13 -> true
| R14 -> true
| R15 -> true
| X8 -> win64
| X9 -> win64
| X10 -> win64
| X11 -> win64
| X12 -> win64
| X13 -> win64
| X14 -> win64
| X15 -> win64
| _ -> false

(** val int_caller_save_regs : mreg list **)

let int_caller_save_regs =
  if ptr64
  then if win64
       then AX :: (CX :: (DX :: (R8 :: (R9 :: (R10 :: (R11 :: []))))))
       else AX :: (CX :: (DX :: (SI :: (DI :: (R8 :: (R9 :: (R10 :: (R11 :: []))))))))
  else AX :: (CX :: (DX :: []))

(** val float_caller_save_regs : mreg list **)

let float_caller_save_regs =
  if ptr64
  then if win64
       then X0 :: (X1 :: (X2 :: (X3 :: (X4 :: (X5 :: [])))))
       else X0 :: (X1 :: (X2 :: (X3 :: (X4 :: (X5 :: (X6 :: (X7 :: (X8 :: (X9 :: (X10 :: (X11 :: (X12 :: (X13 :: (X14 :: (X15 :: [])))))))))))))))
  else X0 :: (X1 :: (X2 :: (X3 :: (X4 :: (X5 :: (X6 :: (X7 :: [])))))))

(** val int_callee_save_regs : mreg list **)

let int_callee_save_regs =
  if ptr64
  then if win64
       then BX :: (SI :: (DI :: (BP :: (R12 :: (R13 :: (R14 :: (R15 :: [])))))))
       else BX :: (BP :: (R12 :: (R13 :: (R14 :: (R15 :: [])))))
  else BX :: (SI :: (DI :: (BP :: [])))

(** val float_callee_save_regs : mreg list **)

let float_callee_save_regs =
  if (&&) ptr64 win64
  then X6 :: (X7 :: (X8 :: (X9 :: (X10 :: (X11 :: (X12 :: (X13 :: (X14 :: (X15 :: [])))))))))
  else []

(** val destroyed_at_call : mreg list **)

let destroyed_at_call =
  filter (fun r -> negb (is_callee_save r)) all_mregs

(** val is_float_reg : mreg -> bool **)

let is_float_reg = function
| AX -> false
| BX -> false
| CX -> false
| DX -> false
| SI -> false
| DI -> false
| BP -> false
| R8 -> false
| R9 -> false
| R10 -> false
| R11 -> false
| R12 -> false
| R13 -> false
| R14 -> false
| R15 -> false
| _ -> true

(** val dummy_int_reg : mreg **)

let dummy_int_reg =
  AX

(** val dummy_float_reg : mreg **)

let dummy_float_reg =
  X0

(** val callee_save_type : mreg -> typ **)

let callee_save_type =
  mreg_type

(** val loc_result_32 : signature -> mreg rpair **)

let loc_result_32 s =
  match proj_sig_res s with
  | Tint -> One AX
  | Tlong -> Twolong (DX, AX)
  | Tany32 -> One AX
  | Tany64 -> One X0
  | _ -> One FP0

(** val loc_result_64 : signature -> mreg rpair **)

let loc_result_64 s =
  match proj_sig_res s with
  | Tfloat -> One X0
  | Tsingle -> One X0
  | _ -> One AX

(** val loc_result : signature -> mreg rpair **)

let loc_result =
  if ptr64 then loc_result_64 else loc_result_32

(** val loc_arguments_32 : typ list -> coq_Z -> loc rpair list **)

let rec loc_arguments_32 tyl ofs =
  match tyl with
  | [] -> []
  | ty :: tys ->
    (match ty with
     | Tlong ->
       Twolong ((S (Outgoing, (Z.add ofs (Zpos Coq_xH)), Tint)), (S
         (Outgoing, ofs, Tint)))
     | _ -> One (S (Outgoing, ofs, ty))) :: (loc_arguments_32 tys
                                              (Z.add ofs (typesize ty)))

(** val int_param_regs_elf64 : mreg list **)

let int_param_regs_elf64 =
  DI :: (SI :: (DX :: (CX :: (R8 :: (R9 :: [])))))

(** val float_param_regs_elf64 : mreg list **)

let float_param_regs_elf64 =
  X0 :: (X1 :: (X2 :: (X3 :: (X4 :: (X5 :: (X6 :: (X7 :: [])))))))

(** val loc_arguments_elf64 :
    typ list -> coq_Z -> coq_Z -> coq_Z -> loc rpair list **)

let rec loc_arguments_elf64 tyl ir fr ofs =
  match tyl with
  | [] -> []
  | ty :: tys ->
    (match ty with
     | Tfloat ->
       (match list_nth_z float_param_regs_elf64 fr with
        | Some freg ->
          (One (R
            freg)) :: (loc_arguments_elf64 tys ir (Z.add fr (Zpos Coq_xH))
                        ofs)
        | None ->
          (One (S (Outgoing, ofs,
            ty))) :: (loc_arguments_elf64 tys ir fr
                       (Z.add ofs (Zpos (Coq_xO Coq_xH)))))
     | Tsingle ->
       (match list_nth_z float_param_regs_elf64 fr with
        | Some freg ->
          (One (R
            freg)) :: (loc_arguments_elf64 tys ir (Z.add fr (Zpos Coq_xH))
                        ofs)
        | None ->
          (One (S (Outgoing, ofs,
            ty))) :: (loc_arguments_elf64 tys ir fr
                       (Z.add ofs (Zpos (Coq_xO Coq_xH)))))
     | _ ->
       (match list_nth_z int_param_regs_elf64 ir with
        | Some ireg ->
          (One (R
            ireg)) :: (loc_arguments_elf64 tys (Z.add ir (Zpos Coq_xH)) fr
                        ofs)
        | None ->
          (One (S (Outgoing, ofs,
            ty))) :: (loc_arguments_elf64 tys ir fr
                       (Z.add ofs (Zpos (Coq_xO Coq_xH))))))

(** val int_param_regs_win64 : mreg list **)

let int_param_regs_win64 =
  CX :: (DX :: (R8 :: (R9 :: [])))

(** val float_param_regs_win64 : mreg list **)

let float_param_regs_win64 =
  X0 :: (X1 :: (X2 :: (X3 :: [])))

(** val loc_arguments_win64 : typ list -> coq_Z -> coq_Z -> loc rpair list **)

let rec loc_arguments_win64 tyl r ofs =
  match tyl with
  | [] -> []
  | ty :: tys ->
    (match ty with
     | Tfloat ->
       (match list_nth_z float_param_regs_win64 r with
        | Some freg ->
          (One (R
            freg)) :: (loc_arguments_win64 tys (Z.add r (Zpos Coq_xH)) ofs)
        | None ->
          (One (S (Outgoing, ofs,
            ty))) :: (loc_arguments_win64 tys r
                       (Z.add ofs (Zpos (Coq_xO Coq_xH)))))
     | Tsingle ->
       (match list_nth_z float_param_regs_win64 r with
        | Some freg ->
          (One (R
            freg)) :: (loc_arguments_win64 tys (Z.add r (Zpos Coq_xH)) ofs)
        | None ->
          (One (S (Outgoing, ofs,
            ty))) :: (loc_arguments_win64 tys r
                       (Z.add ofs (Zpos (Coq_xO Coq_xH)))))
     | _ ->
       (match list_nth_z int_param_regs_win64 r with
        | Some ireg ->
          (One (R
            ireg)) :: (loc_arguments_win64 tys (Z.add r (Zpos Coq_xH)) ofs)
        | None ->
          (One (S (Outgoing, ofs,
            ty))) :: (loc_arguments_win64 tys r
                       (Z.add ofs (Zpos (Coq_xO Coq_xH))))))

(** val loc_arguments : signature -> loc rpair list **)

let loc_arguments s =
  if ptr64
  then if win64
       then loc_arguments_win64 s.sig_args Z0 Z0
       else loc_arguments_elf64 s.sig_args Z0 Z0 Z0
  else loc_arguments_32 s.sig_args Z0

(** val return_value_needs_normalization : rettype -> bool **)

let return_value_needs_normalization = function
| Tret _ -> false
| Tvoid -> false
| _ -> true

(** val parameter_needs_normalization : rettype -> bool **)

let parameter_needs_normalization _ =
  false
