open AST
open BinInt
open BinNums
open Conventions
open Coqlib
open Datatypes
open Errors
open Integers
open Maps
open Op
open RTL
open Registers
open Unityping

type __ = Obj.t

type regenv = reg -> typ

module RTLtypes =
 struct
  type t = typ

  (** val eq : typ -> typ -> bool **)

  let eq =
    typ_eq

  (** val default : typ **)

  let default =
    Tint
 end

module S = UniSolver(RTLtypes)

(** val check_successor : coq_function -> node -> unit res **)

let check_successor f s =
  match PTree.get s f.fn_code with
  | Some _ -> OK ()
  | None ->
    Error ((MSG
      ('b'::('a'::('d'::(' '::('s'::('u'::('c'::('c'::('e'::('s'::('s'::('o'::('r'::(' '::[]))))))))))))))) :: ((POS
      s) :: []))

(** val check_successors : coq_function -> node list -> unit res **)

let rec check_successors f = function
| [] -> OK ()
| s1 :: sl' ->
  (match check_successor f s1 with
   | OK _ -> check_successors f sl'
   | Error msg0 -> Error msg0)

(** val type_ros : S.typenv -> (reg, ident) sum -> S.typenv res **)

let type_ros e = function
| Coq_inl r -> S.set e r coq_Tptr
| Coq_inr _ -> OK e

(** val is_move : operation -> bool **)

let is_move = function
| Omove -> true
| _ -> false

(** val type_expect : S.typenv -> typ -> typ -> S.typenv res **)

let type_expect e t1 t2 =
  if typ_eq t1 t2
  then OK e
  else Error
         (msg
           ('u'::('n'::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::[]))))))))))))))))

(** val type_builtin_arg :
    S.typenv -> reg builtin_arg -> typ -> S.typenv res **)

let type_builtin_arg e a ty =
  match a with
  | BA r -> S.set e r ty
  | BA_int _ -> type_expect e ty Tint
  | BA_long _ -> type_expect e ty Tlong
  | BA_float _ -> type_expect e ty Tfloat
  | BA_single _ -> type_expect e ty Tsingle
  | BA_loadstack (chunk, _) -> type_expect e ty (type_of_chunk chunk)
  | BA_loadglobal (chunk, _, _) -> type_expect e ty (type_of_chunk chunk)
  | BA_splitlong (_, _) -> type_expect e ty Tlong
  | _ -> type_expect e ty coq_Tptr

(** val type_builtin_args :
    S.typenv -> reg builtin_arg list -> typ list -> S.typenv res **)

let rec type_builtin_args e al tyl =
  match al with
  | [] ->
    (match tyl with
     | [] -> OK e
     | _ :: _ ->
       Error
         (msg
           ('b'::('u'::('i'::('l'::('t'::('i'::('n'::(' '::('a'::('r'::('i'::('t'::('y'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))
  | a1 :: al0 ->
    (match tyl with
     | [] ->
       Error
         (msg
           ('b'::('u'::('i'::('l'::('t'::('i'::('n'::(' '::('a'::('r'::('i'::('t'::('y'::(' '::('m'::('i'::('s'::('m'::('a'::('t'::('c'::('h'::[])))))))))))))))))))))))
     | ty1 :: tyl0 ->
       (match type_builtin_arg e a1 ty1 with
        | OK x -> type_builtin_args x al0 tyl0
        | Error msg0 -> Error msg0))

(** val type_builtin_res :
    S.typenv -> reg builtin_res -> typ -> S.typenv res **)

let type_builtin_res e a ty =
  match a with
  | BR r -> S.set e r ty
  | _ -> type_expect e ty Tint

(** val type_instr :
    coq_function -> S.typenv -> instruction -> S.typenv res **)

let type_instr f e = function
| Inop s ->
  (match check_successor f s with
   | OK _ -> OK e
   | Error msg0 -> Error msg0)
| Iop (op, args, res0, s) ->
  (match check_successor f s with
   | OK _ ->
     if is_move op
     then (match args with
           | [] ->
             Error
               (msg
                 ('i'::('l'::('l'::('-'::('f'::('o'::('r'::('m'::('e'::('d'::(' '::('m'::('o'::('v'::('e'::[]))))))))))))))))
           | arg :: l ->
             (match l with
              | [] ->
                (match S.move e res0 arg with
                 | OK p -> let (_, y) = p in OK y
                 | Error msg0 -> Error msg0)
              | _ :: _ ->
                Error
                  (msg
                    ('i'::('l'::('l'::('-'::('f'::('o'::('r'::('m'::('e'::('d'::(' '::('m'::('o'::('v'::('e'::[]))))))))))))))))))
     else let (targs, tres) = type_of_operation op in
          (match S.set_list e args targs with
           | OK x -> S.set x res0 tres
           | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Iload (chunk, addr, args, dst, s) ->
  (match check_successor f s with
   | OK _ ->
     (match S.set_list e args (type_of_addressing addr) with
      | OK x -> S.set x dst (type_of_chunk chunk)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Istore (chunk, addr, args, src, s) ->
  (match check_successor f s with
   | OK _ ->
     (match S.set_list e args (type_of_addressing addr) with
      | OK x -> S.set x src (type_of_chunk chunk)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Icall (sig0, ros, args, res0, s) ->
  (match check_successor f s with
   | OK _ ->
     (match type_ros e ros with
      | OK x ->
        (match S.set_list x args sig0.sig_args with
         | OK x0 -> S.set x0 res0 (proj_sig_res sig0)
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Itailcall (sig0, ros, args) ->
  (match type_ros e ros with
   | OK x ->
     (match S.set_list x args sig0.sig_args with
      | OK x0 ->
        if rettype_eq sig0.sig_res f.fn_sig.sig_res
        then if tailcall_is_possible sig0
             then OK x0
             else Error
                    (msg
                      ('t'::('a'::('i'::('l'::('c'::('a'::('l'::('l'::(' '::('n'::('o'::('t'::(' '::('p'::('o'::('s'::('s'::('i'::('b'::('l'::('e'::[]))))))))))))))))))))))
        else Error
               (msg
                 ('b'::('a'::('d'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('i'::('n'::(' '::('t'::('a'::('i'::('l'::('c'::('a'::('l'::('l'::[]))))))))))))))))))))))))))))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ibuiltin (ef, args, res0, s) ->
  let sig0 = ef_sig ef in
  (match check_successor f s with
   | OK _ ->
     (match match ef with
            | EF_annot (_, _, _) -> OK e
            | EF_debug (_, _, _) -> OK e
            | _ -> type_builtin_args e args sig0.sig_args with
      | OK x -> type_builtin_res x res0 (proj_sig_res sig0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Icond (cond, args, s1, s2) ->
  (match check_successor f s1 with
   | OK _ ->
     (match check_successor f s2 with
      | OK _ -> S.set_list e args (type_of_condition cond)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ijumptable (arg, tbl) ->
  (match check_successors f tbl with
   | OK _ ->
     (match S.set e arg Tint with
      | OK x ->
        if zle (Z.mul (list_length_z tbl) (Zpos (Coq_xO (Coq_xO Coq_xH))))
             Int.max_unsigned
        then OK x
        else Error
               (msg
                 ('j'::('u'::('m'::('p'::('t'::('a'::('b'::('l'::('e'::(' '::('t'::('o'::('o'::(' '::('b'::('i'::('g'::[]))))))))))))))))))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Ireturn optres ->
  (match optres with
   | Some r ->
     if rettype_eq f.fn_sig.sig_res Tvoid
     then Error
            (msg
              ('b'::('a'::('d'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::[])))))))))))
     else S.set e r (proj_sig_res f.fn_sig)
   | None ->
     if rettype_eq f.fn_sig.sig_res Tvoid
     then OK e
     else Error
            (msg
              ('b'::('a'::('d'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::[]))))))))))))

(** val type_code : coq_function -> S.typenv -> S.typenv res **)

let type_code f e =
  PTree.fold (fun re pc i ->
    match re with
    | OK e0 ->
      (match type_instr f e0 i with
       | OK e' -> OK e'
       | Error msg0 ->
         Error ((MSG ('A'::('t'::(' '::('P'::('C'::(' '::[]))))))) :: ((POS
           pc) :: ((MSG (':'::(' '::[]))) :: msg0))))
    | Error _ -> re) f.fn_code (OK e)

(** val check_params_norepet : reg list -> unit res **)

let check_params_norepet params =
  if list_norepet_dec Reg.eq params
  then OK ()
  else Error
         (msg
           ('d'::('u'::('p'::('l'::('i'::('c'::('a'::('t'::('e'::(' '::('p'::('a'::('r'::('a'::('m'::('e'::('t'::('e'::('r'::('s'::[])))))))))))))))))))))

(** val type_function : coq_function -> regenv res **)

let type_function f =
  match type_code f S.initial with
  | OK x ->
    (match S.set_list x f.fn_params f.fn_sig.sig_args with
     | OK x0 ->
       (match S.solve x0 with
        | OK x1 ->
          (match check_params_norepet f.fn_params with
           | OK _ ->
             (match check_successor f f.fn_entrypoint with
              | OK _ -> OK x1
              | Error msg0 -> Error msg0)
           | Error msg0 -> Error msg0)
        | Error msg0 -> Error msg0)
     | Error msg0 -> Error msg0)
  | Error msg0 -> Error msg0
