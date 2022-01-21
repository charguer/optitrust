open AST
open Archi
open BinNums
open Coqlib
open Datatypes
open List0
open Op
open String0

type mreg =
| AX
| BX
| CX
| DX
| SI
| DI
| BP
| R8
| R9
| R10
| R11
| R12
| R13
| R14
| R15
| X0
| X1
| X2
| X3
| X4
| X5
| X6
| X7
| X8
| X9
| X10
| X11
| X12
| X13
| X14
| X15
| FP0

(** val mreg_eq : mreg -> mreg -> bool **)

let mreg_eq r1 r2 =
  match r1 with
  | AX -> (match r2 with
           | AX -> true
           | _ -> false)
  | BX -> (match r2 with
           | BX -> true
           | _ -> false)
  | CX -> (match r2 with
           | CX -> true
           | _ -> false)
  | DX -> (match r2 with
           | DX -> true
           | _ -> false)
  | SI -> (match r2 with
           | SI -> true
           | _ -> false)
  | DI -> (match r2 with
           | DI -> true
           | _ -> false)
  | BP -> (match r2 with
           | BP -> true
           | _ -> false)
  | R8 -> (match r2 with
           | R8 -> true
           | _ -> false)
  | R9 -> (match r2 with
           | R9 -> true
           | _ -> false)
  | R10 -> (match r2 with
            | R10 -> true
            | _ -> false)
  | R11 -> (match r2 with
            | R11 -> true
            | _ -> false)
  | R12 -> (match r2 with
            | R12 -> true
            | _ -> false)
  | R13 -> (match r2 with
            | R13 -> true
            | _ -> false)
  | R14 -> (match r2 with
            | R14 -> true
            | _ -> false)
  | R15 -> (match r2 with
            | R15 -> true
            | _ -> false)
  | X0 -> (match r2 with
           | X0 -> true
           | _ -> false)
  | X1 -> (match r2 with
           | X1 -> true
           | _ -> false)
  | X2 -> (match r2 with
           | X2 -> true
           | _ -> false)
  | X3 -> (match r2 with
           | X3 -> true
           | _ -> false)
  | X4 -> (match r2 with
           | X4 -> true
           | _ -> false)
  | X5 -> (match r2 with
           | X5 -> true
           | _ -> false)
  | X6 -> (match r2 with
           | X6 -> true
           | _ -> false)
  | X7 -> (match r2 with
           | X7 -> true
           | _ -> false)
  | X8 -> (match r2 with
           | X8 -> true
           | _ -> false)
  | X9 -> (match r2 with
           | X9 -> true
           | _ -> false)
  | X10 -> (match r2 with
            | X10 -> true
            | _ -> false)
  | X11 -> (match r2 with
            | X11 -> true
            | _ -> false)
  | X12 -> (match r2 with
            | X12 -> true
            | _ -> false)
  | X13 -> (match r2 with
            | X13 -> true
            | _ -> false)
  | X14 -> (match r2 with
            | X14 -> true
            | _ -> false)
  | X15 -> (match r2 with
            | X15 -> true
            | _ -> false)
  | FP0 -> (match r2 with
            | FP0 -> true
            | _ -> false)

(** val all_mregs : mreg list **)

let all_mregs =
  AX :: (BX :: (CX :: (DX :: (SI :: (DI :: (BP :: (R8 :: (R9 :: (R10 :: (R11 :: (R12 :: (R13 :: (R14 :: (R15 :: (X0 :: (X1 :: (X2 :: (X3 :: (X4 :: (X5 :: (X6 :: (X7 :: (X8 :: (X9 :: (X10 :: (X11 :: (X12 :: (X13 :: (X14 :: (X15 :: (FP0 :: [])))))))))))))))))))))))))))))))

(** val mreg_type : mreg -> typ **)

let mreg_type = function
| AX -> if ptr64 then Tany64 else Tany32
| BX -> if ptr64 then Tany64 else Tany32
| CX -> if ptr64 then Tany64 else Tany32
| DX -> if ptr64 then Tany64 else Tany32
| SI -> if ptr64 then Tany64 else Tany32
| DI -> if ptr64 then Tany64 else Tany32
| BP -> if ptr64 then Tany64 else Tany32
| _ -> Tany64

module IndexedMreg =
 struct
  type t = mreg

  (** val eq : mreg -> mreg -> bool **)

  let eq =
    mreg_eq

  (** val index : mreg -> positive **)

  let index = function
  | AX -> Coq_xH
  | BX -> Coq_xO Coq_xH
  | CX -> Coq_xI Coq_xH
  | DX -> Coq_xO (Coq_xO Coq_xH)
  | SI -> Coq_xI (Coq_xO Coq_xH)
  | DI -> Coq_xO (Coq_xI Coq_xH)
  | BP -> Coq_xI (Coq_xI Coq_xH)
  | R8 -> Coq_xO (Coq_xO (Coq_xO Coq_xH))
  | R9 -> Coq_xI (Coq_xO (Coq_xO Coq_xH))
  | R10 -> Coq_xO (Coq_xI (Coq_xO Coq_xH))
  | R11 -> Coq_xI (Coq_xI (Coq_xO Coq_xH))
  | R12 -> Coq_xO (Coq_xO (Coq_xI Coq_xH))
  | R13 -> Coq_xI (Coq_xO (Coq_xI Coq_xH))
  | R14 -> Coq_xO (Coq_xI (Coq_xI Coq_xH))
  | R15 -> Coq_xI (Coq_xI (Coq_xI Coq_xH))
  | X0 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
  | X1 -> Coq_xI (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
  | X2 -> Coq_xO (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
  | X3 -> Coq_xI (Coq_xI (Coq_xO (Coq_xO Coq_xH)))
  | X4 -> Coq_xO (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
  | X5 -> Coq_xI (Coq_xO (Coq_xI (Coq_xO Coq_xH)))
  | X6 -> Coq_xO (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
  | X7 -> Coq_xI (Coq_xI (Coq_xI (Coq_xO Coq_xH)))
  | X8 -> Coq_xO (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
  | X9 -> Coq_xI (Coq_xO (Coq_xO (Coq_xI Coq_xH)))
  | X10 -> Coq_xO (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
  | X11 -> Coq_xI (Coq_xI (Coq_xO (Coq_xI Coq_xH)))
  | X12 -> Coq_xO (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
  | X13 -> Coq_xI (Coq_xO (Coq_xI (Coq_xI Coq_xH)))
  | X14 -> Coq_xO (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
  | X15 -> Coq_xI (Coq_xI (Coq_xI (Coq_xI Coq_xH)))
  | FP0 -> Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
 end

(** val is_stack_reg : mreg -> bool **)

let is_stack_reg = function
| FP0 -> true
| _ -> false

(** val register_names : (char list * mreg) list **)

let register_names =
  (('R'::('A'::('X'::[]))), AX) :: ((('R'::('B'::('X'::[]))),
    BX) :: ((('R'::('C'::('X'::[]))), CX) :: ((('R'::('D'::('X'::[]))),
    DX) :: ((('R'::('S'::('I'::[]))), SI) :: ((('R'::('D'::('I'::[]))),
    DI) :: ((('R'::('B'::('P'::[]))), BP) :: ((('E'::('A'::('X'::[]))),
    AX) :: ((('E'::('B'::('X'::[]))), BX) :: ((('E'::('C'::('X'::[]))),
    CX) :: ((('E'::('D'::('X'::[]))), DX) :: ((('E'::('S'::('I'::[]))),
    SI) :: ((('E'::('D'::('I'::[]))), DI) :: ((('E'::('B'::('P'::[]))),
    BP) :: ((('R'::('8'::[])), R8) :: ((('R'::('9'::[])),
    R9) :: ((('R'::('1'::('0'::[]))), R10) :: ((('R'::('1'::('1'::[]))),
    R11) :: ((('R'::('1'::('2'::[]))), R12) :: ((('R'::('1'::('3'::[]))),
    R13) :: ((('R'::('1'::('4'::[]))), R14) :: ((('R'::('1'::('5'::[]))),
    R15) :: ((('X'::('M'::('M'::('0'::[])))),
    X0) :: ((('X'::('M'::('M'::('1'::[])))),
    X1) :: ((('X'::('M'::('M'::('2'::[])))),
    X2) :: ((('X'::('M'::('M'::('3'::[])))),
    X3) :: ((('X'::('M'::('M'::('4'::[])))),
    X4) :: ((('X'::('M'::('M'::('5'::[])))),
    X5) :: ((('X'::('M'::('M'::('6'::[])))),
    X6) :: ((('X'::('M'::('M'::('7'::[])))),
    X7) :: ((('X'::('M'::('M'::('8'::[])))),
    X8) :: ((('X'::('M'::('M'::('9'::[])))),
    X9) :: ((('X'::('M'::('M'::('1'::('0'::[]))))),
    X10) :: ((('X'::('M'::('M'::('1'::('1'::[]))))),
    X11) :: ((('X'::('M'::('M'::('1'::('2'::[]))))),
    X12) :: ((('X'::('M'::('M'::('1'::('3'::[]))))),
    X13) :: ((('X'::('M'::('M'::('1'::('4'::[]))))),
    X14) :: ((('X'::('M'::('M'::('1'::('5'::[]))))),
    X15) :: ((('S'::('T'::('0'::[]))),
    FP0) :: []))))))))))))))))))))))))))))))))))))))

(** val register_by_name : char list -> mreg option **)

let register_by_name s =
  let rec assoc = function
  | [] -> None
  | p :: l' ->
    let (s1, r1) = p in if string_dec s s1 then Some r1 else assoc l'
  in assoc register_names

(** val destroyed_by_op : operation -> mreg list **)

let destroyed_by_op = function
| Ocast8signed -> AX :: []
| Ocast8unsigned -> AX :: []
| Omulhs -> AX :: (DX :: [])
| Omulhu -> AX :: (DX :: [])
| Odiv -> AX :: (DX :: [])
| Odivu -> AX :: (DX :: [])
| Omod -> AX :: (DX :: [])
| Omodu -> AX :: (DX :: [])
| Oshrximm _ -> CX :: []
| Omullhs -> AX :: (DX :: [])
| Omullhu -> AX :: (DX :: [])
| Odivl -> AX :: (DX :: [])
| Odivlu -> AX :: (DX :: [])
| Omodl -> AX :: (DX :: [])
| Omodlu -> AX :: (DX :: [])
| Oshrxlimm _ -> DX :: []
| Ocmp _ -> AX :: (CX :: [])
| _ -> []

(** val destroyed_by_load : memory_chunk -> addressing -> mreg list **)

let destroyed_by_load _ _ =
  []

(** val destroyed_by_store : memory_chunk -> addressing -> mreg list **)

let destroyed_by_store chunk _ =
  match chunk with
  | Mint8signed -> if ptr64 then [] else AX :: (CX :: [])
  | Mint8unsigned -> if ptr64 then [] else AX :: (CX :: [])
  | _ -> []

(** val destroyed_by_cond : condition -> mreg list **)

let destroyed_by_cond _ =
  []

(** val destroyed_by_jumptable : mreg list **)

let destroyed_by_jumptable =
  AX :: (DX :: [])

(** val destroyed_by_clobber : char list list -> mreg list **)

let rec destroyed_by_clobber = function
| [] -> []
| c1 :: cl0 ->
  (match register_by_name c1 with
   | Some r -> r :: (destroyed_by_clobber cl0)
   | None -> destroyed_by_clobber cl0)

(** val destroyed_by_builtin : external_function -> mreg list **)

let destroyed_by_builtin = function
| EF_builtin (name, _) ->
  if string_dec name
       ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('v'::('a'::('_'::('s'::('t'::('a'::('r'::('t'::[]))))))))))))))))))
  then AX :: []
  else if (||)
            ((fun x -> x)
              (string_dec name
                ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('w'::('r'::('i'::('t'::('e'::('1'::('6'::('_'::('r'::('e'::('v'::('e'::('r'::('s'::('e'::('d'::[]))))))))))))))))))))))))))))
            ((fun x -> x)
              (string_dec name
                ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('w'::('r'::('i'::('t'::('e'::('3'::('2'::('_'::('r'::('e'::('v'::('e'::('r'::('s'::('e'::('d'::[]))))))))))))))))))))))))))))
       then CX :: (DX :: [])
       else []
| EF_vstore chunk ->
  (match chunk with
   | Mint8signed -> if ptr64 then [] else AX :: (CX :: [])
   | Mint8unsigned -> if ptr64 then [] else AX :: (CX :: [])
   | _ -> [])
| EF_memcpy (sz, _) ->
  if zle sz (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then CX :: (X7 :: [])
  else CX :: (SI :: (DI :: []))
| EF_inline_asm (_, _, clob) -> destroyed_by_clobber clob
| _ -> []

(** val destroyed_at_function_entry : mreg list **)

let destroyed_at_function_entry =
  AX :: (FP0 :: [])

(** val destroyed_by_setstack : typ -> mreg list **)

let destroyed_by_setstack = function
| Tfloat -> FP0 :: []
| Tsingle -> FP0 :: []
| _ -> []

(** val destroyed_at_indirect_call : mreg list **)

let destroyed_at_indirect_call =
  AX :: []

(** val temp_for_parent_frame : mreg **)

let temp_for_parent_frame =
  AX

(** val mregs_for_operation : operation -> mreg option list * mreg option **)

let mregs_for_operation = function
| Omulhs -> (((Some AX) :: (None :: [])), (Some DX))
| Omulhu -> (((Some AX) :: (None :: [])), (Some DX))
| Odiv -> (((Some AX) :: ((Some CX) :: [])), (Some AX))
| Odivu -> (((Some AX) :: ((Some CX) :: [])), (Some AX))
| Omod -> (((Some AX) :: ((Some CX) :: [])), (Some DX))
| Omodu -> (((Some AX) :: ((Some CX) :: [])), (Some DX))
| Oshl -> ((None :: ((Some CX) :: [])), None)
| Oshr -> ((None :: ((Some CX) :: [])), None)
| Oshrximm _ -> (((Some AX) :: []), (Some AX))
| Oshru -> ((None :: ((Some CX) :: [])), None)
| Omullhs -> (((Some AX) :: (None :: [])), (Some DX))
| Omullhu -> (((Some AX) :: (None :: [])), (Some DX))
| Odivl -> (((Some AX) :: ((Some CX) :: [])), (Some AX))
| Odivlu -> (((Some AX) :: ((Some CX) :: [])), (Some AX))
| Omodl -> (((Some AX) :: ((Some CX) :: [])), (Some DX))
| Omodlu -> (((Some AX) :: ((Some CX) :: [])), (Some DX))
| Oshll -> ((None :: ((Some CX) :: [])), None)
| Oshrl -> ((None :: ((Some CX) :: [])), None)
| Oshrxlimm _ -> (((Some AX) :: []), (Some AX))
| Oshrlu -> ((None :: ((Some CX) :: [])), None)
| _ -> ([], None)

(** val mregs_for_builtin :
    external_function -> mreg option list * mreg option list **)

let mregs_for_builtin = function
| EF_builtin (name, _) ->
  if string_dec name
       ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('n'::('e'::('g'::('l'::[]))))))))))))))
  then (((Some DX) :: ((Some AX) :: [])), ((Some DX) :: ((Some AX) :: [])))
  else if (||)
            ((fun x -> x)
              (string_dec name
                ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('a'::('d'::('d'::('l'::[]))))))))))))))))
            ((fun x -> x)
              (string_dec name
                ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('s'::('u'::('b'::('l'::[]))))))))))))))))
       then (((Some DX) :: ((Some AX) :: ((Some CX) :: ((Some BX) :: [])))),
              ((Some DX) :: ((Some AX) :: [])))
       else if string_dec name
                 ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('m'::('u'::('l'::('l'::[]))))))))))))))
            then (((Some AX) :: ((Some DX) :: [])), ((Some DX) :: ((Some
                   AX) :: [])))
            else if string_dec name
                      ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('v'::('a'::('_'::('s'::('t'::('a'::('r'::('t'::[]))))))))))))))))))
                 then (((Some DX) :: []), [])
                 else if (&&) (negb ptr64)
                           ((fun x -> x)
                             (string_dec name
                               ('_'::('_'::('b'::('u'::('i'::('l'::('t'::('i'::('n'::('_'::('b'::('s'::('w'::('a'::('p'::('6'::('4'::[])))))))))))))))))))
                      then (((Some AX) :: ((Some DX) :: [])), ((Some
                             DX) :: ((Some AX) :: [])))
                      else ([], [])
| EF_memcpy (sz, _) ->
  if zle sz (Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH))))))
  then (((Some AX) :: ((Some DX) :: [])), [])
  else (((Some DI) :: ((Some SI) :: [])), [])
| _ -> ([], [])

(** val two_address_op : operation -> bool **)

let two_address_op = function
| Omove -> false
| Ointconst _ -> false
| Olongconst _ -> false
| Ofloatconst _ -> false
| Osingleconst _ -> false
| Oindirectsymbol _ -> false
| Ocast8signed -> false
| Ocast8unsigned -> false
| Ocast16signed -> false
| Ocast16unsigned -> false
| Omulhs -> false
| Omulhu -> false
| Odiv -> false
| Odivu -> false
| Omod -> false
| Omodu -> false
| Oshrximm _ -> false
| Olea _ -> false
| Ocast32signed -> false
| Ocast32unsigned -> false
| Omullhs -> false
| Omullhu -> false
| Odivl -> false
| Odivlu -> false
| Omodl -> false
| Omodlu -> false
| Oshrxlimm _ -> false
| Oleal _ -> false
| Osingleoffloat -> false
| Ofloatofsingle -> false
| Ointoffloat -> false
| Ofloatofint -> false
| Ointofsingle -> false
| Osingleofint -> false
| Olongoffloat -> false
| Ofloatoflong -> false
| Olongofsingle -> false
| Osingleoflong -> false
| Ocmp _ -> false
| _ -> true

(** val builtin_constraints :
    external_function -> builtin_arg_constraint list **)

let builtin_constraints = function
| EF_vload _ -> OK_addressing :: []
| EF_vstore _ -> OK_addressing :: (OK_default :: [])
| EF_memcpy (_, _) -> OK_addrstack :: (OK_addrstack :: [])
| EF_annot (_, _, targs) -> map (fun _ -> OK_all) targs
| EF_debug (_, _, targs) -> map (fun _ -> OK_all) targs
| _ -> []
